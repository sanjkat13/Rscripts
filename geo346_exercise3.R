# packages ----------------------------------------------------------------
library(dplyr) # data wrangling with pipe syntax
library(tidyverse) # data wrangling
library(r5r) # multimodal transportation modeling
library(gtfstools) # processing, analyzing GTFS files
library(tigris) # download census geographies
library(sf) # simple features data package
library(sp) # spatial data package
library(units) # manage measurement units
library(ggplot2) # creating plots
library(psych) # summary statistics




# download cc and create hex ----------------------------------------------

cc_boundary_utm16N <- counties(state="IL", cb = TRUE) %>%
  filter(GEOID == "17031") %>%
  select(GEOID, NAME) %>%
  st_transform(crs = 26916)

# create regular hexagonal grid
cc_hexagons_utm16N <- cc_boundary_utm16N %>%
  st_make_grid(cellsize = 1609, square = FALSE) %>%
  st_transform(crs=26916) %>%
  st_cast("POLYGON") %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(hexid = row_number()) %>%
  st_intersection(cc_boundary_utm16N) %>%
  st_as_sf()

# convert Cook County and hexagons into world geographic coordinate system for multimodal accessibility calculations
cc_boundary_wgs84 <- cc_boundary_utm16N %>%
  st_transform(crs = 4326)

cc_hexagons_wgs84 <- cc_hexagons_utm16N %>%
  st_transform(crs = 4326)

# centroids of hexagons
# N=1,210
cc_hexagons_centroids_wgs84 <- cc_hexagons_wgs84 %>%
  st_centroid()

cc_hexagons_centroids_wgs84 <- cc_hexagons_centroids_wgs84 %>%
  bind_cols(st_coordinates(cc_hexagons_centroids_wgs84)) %>%
  rename(lat = Y,
         lon = X)

cc_hexagons_centroids_utm16N <- cc_hexagons_centroids_wgs84 %>%
  st_transform(crs=26916)

# save as shapefiles to layers folder for mapping
st_write(cc_boundary_utm16N, "layers/cc_boundary.shp", append=FALSE)
st_write(cc_hexagons_utm16N, "layers/cc_hexagons.shp", append=FALSE)
st_write(cc_hexagons_centroids_utm16N, "layers/cc_hexagons_centroids.shp", append=FALSE)


# download destination data -----------------------------------------------

cc_hospitals_wgs84 <- st_read("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/Hospitals_1/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>%
  filter(COUNTYFIPS=="17031") %>%
  select(-c(VAL_DATE,SOURCEDATE))

cc_hospitals_utm16N <- cc_hospitals_wgs84 %>%
  st_transform(crs = 26916)

st_write(cc_hospitals_utm16N, "layers/cc_hospitals.shp", append=FALSE)




# container based approach ------------------------------------------------

cc_hexagons_hospitals <- cc_hexagons_utm16N %>%
  st_join(cc_hospitals_utm16N %>% select(ID)) %>%
  st_drop_geometry() %>%
  mutate(value = if_else(!is.na(ID),1,0)) %>%
  group_by(hexid) %>%
  summarise(count = sum(value))

# create histogram
ggplot(cc_hexagons_hospitals, aes(x=count)) +
  geom_histogram() +
  xlab("count") +
  ylab("frequency")

# create data summary
describe(cc_hexagons_hospitals$count) %>% t()

###limitations:
  #locations near the border of a hexagon might cause
    #the calculated value to be 0 even though it's close by
    #vice versa to the opposite side of the container
write_csv(cc_hexagons_hospitals, "data/cc_hexagons_hospitals_container.csv")



# (Cartesian) Distance-based approach -------------------------------------
#calculate distance from every hexagon to every hospital
#A distance-based approach measures and compares
  #the distances between origin and destination locations.

# create an origin destination (O-D) matrix with distance units in miles
od_matrix_distance <- st_distance(cc_hexagons_centroids_utm16N, cc_hospitals_utm16N) %>%
  as.data.frame() %>%
  drop_units() %>%
  mutate_if(is.numeric, ~ . * 0.000621371) %>%
  bind_cols(cc_hexagons_centroids_utm16N %>%
              st_drop_geometry() %>%
              select(hexid)) %>%
  select(hexid,V1:V76)



# calculate summary distance calculations for each hexagon
od_matrix_distance_sum <- od_matrix_distance %>%
  rowwise() %>%
  mutate(min = min(across(V1:V76)),### find the minimum for each column = closest hospital
         max = max(across(V1:V76)),
         mean = mean(V1:V76),##avg distance to all hospitals
         n_1mi = sum(across(V1:V76) <= 1),
         n_5mi = sum(across(V1:V76) <= 5),
         n_10mi = sum(across(V1:V76) <= 10)) %>% #pick one of these 1/5/10
  select(hexid, min:n_10mi)

# create histogram
ggplot(od_matrix_distance_sum, aes(x=n_10mi)) +
  geom_histogram() +
  xlab("count") +
  ylab("frequency")

# create data summary
describe(od_matrix_distance_sum$n_10mi) %>% t()

#export to csv
write_csv(od_matrix_distance_sum, "data/od_matrix_distance_sum.csv")

### limitations: constraints of study area,
#ignores hospitals right outside cc boundaries





# transportation ----------------------------------------------------------
download.file("https://www.justenvirons.org/geo346/exercise_03/data/chicago_region.pbf", destfile = "data/chicago.pbf")

download.file("https://www.justenvirons.org/geo346/exercise_03/data/gtfs_metra.zip", destfile = "data/gtfs_metra.zip")

download.file("http://www.transitchicago.com/downloads/sch_data/google_transit.zip", destfile = "data/gtfs_cta.zip")

download.file("https://public.pacebus.com/gtfs/gtfs.zip", destfile = "data/gtfs_pace.zip")







