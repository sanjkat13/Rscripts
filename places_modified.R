library(sf)
library(dplyr) 
library(tidyverse)
library(tidycensus)
library(readxl)
library(RColorBrewer)
library(patchwork)
library(ggplot2)
library(biscale)
library(corrplot)
library(scales)

census_api_key("0bad56e1a374c753569328e2535d99d30eb29ead")

# download PLACES data for Cook County by census tract using CDC API
places_tracts <- read.csv("https://chronicdata.cdc.gov/api/views/yjkw-uj5s/rows.csv?accessType=DOWNLOAD&bom=true&format=true") %>%
  filter(StateAbbr=="IL", CountyName=="Cook") %>%
  mutate(geoid10 = as.character(TractFIPS)) %>% #what does mutate or as.character do
  select(geoid10,
         contains("_CrudePrev")) 

# download 2010 Chicago census tract list from Chicago data portal
chicago_tracts <- st_read("https://data.cityofchicago.org/api/geospatial/5jrd-6zik?method=export&format=GeoJSON") %>%
  select(geoid10,
         commarea) %>%
  st_drop_geometry()

# download Cook County census tract geometries, subset to Chicago tracts and join PLACES data 
chicago_tracts_places_geom <- get_acs( #did not know this function
  state = "IL",
  county = "Cook",
  geography = "tract",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2010
) %>%
  select(geoid10 = GEOID,
         pop_2010 = estimate) %>%
  right_join(chicago_tracts, by="geoid10") %>%
  left_join(places_tracts,by="geoid10") %>%
  st_transform(crs=26916)

# write to shapefile
st_write(chicago_tracts_places_geom, "chicago_tracts_places_geom.shp", append=FALSE)



# places sum data dictionary --------------------------------------------------------------

# PLACES data
chicago_places_data_sum <- chicago_tracts_places_geom %>% 
  st_drop_geometry()
chicago_places_data_sum <-write_csv(chicago_places_data_sum, "chicago_places_data")

chicago_places_xl <- read_excel("chicago_places_data.xlsx")



# parks  ------------------------------------------------------------------
chicago_tracts_parks <- get_acs( 
  state = "IL",
  county = "Cook",
  geography = "tract",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2010
) %>%
  select(geoid10 = GEOID,
         pop_2010 = estimate) %>%
  st_transform(crs=26916) %>%
  st_as_sf()

write_sf(chicago_tracts_parks, "chicago_tracts_parks.shp")

# correlation plots -------------------------------------------------------


places <- chicago_tracts_places_geom %>%
  select(geoid10, 
         pop_2010,
         commarea,
         BINGE_CrudePrev,
         GHLTH_CrudePrev,
         BPHIGH_CrudePrev,
         CANCER_CrudePrev,
         CASTHMA_CrudePrev,
         LPA_CrudePrev) %>%
  rename(BINGE = BINGE_CrudePrev,
         GHLTH = GHLTH_CrudePrev,
         BPHIGH = BPHIGH_CrudePrev,
         CANCER = CANCER_CrudePrev,
         ASTHMA = CASTHMA_CrudePrev,
         LPA = LPA_CrudePrev,
         geoid = geoid10
         ) %>%
  mutate(geoid = as.numeric(geoid))


# join parks summary table with places 

parks <- read_xlsx("SummarizedParksBuffer.xlsx") %>%
  mutate(geoid = as.numeric(geoid))
places_m <- places %>%
  mutate(geoid = as.numeric(geoid))


parks_places <- places  %>%
  left_join(parks, by = "geoid") %>%
  mutate(across(geoid = as.numeric(geoid))) 

#----------------------------------------#


# correlation map 1 - binge drinking


# -- Map 1 Bivariate
map1.data <- bi_class(parks_places, 
                      x = BINGE, 
                      y = PercentagePark, 
                      style = "quantile", dim = 3)

map1 <-   ggplot() +
  geom_sf(data = map1.data, 
          mapping = aes(fill = bi_class), 
          color = "grey", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal= "GrPink", dim = 3) + 
  theme_minimal()

# bivariate map 1 legend
map1.legend <- bi_legend(pal = "GrPink",
                         dim = 3,
                         xlab = "Binge Drinking",
                         ylab = "Park Accessibility",
                         size = 5,
                         pad_width = 1) + 
  theme_minimal()

map1.bivariate <- map1 + map1.legend + plot_layout(widths = c(4,1))

ggsave(map1.bivariate,
       filename = "bivariate_maps/map1_bivariate.png",
       scale = 1,
       dpi = 300,
       height = 4,
       width = 8.5,
       units="in")

#----------------------------------#

# -- Map 2 Bivariate
map2.data <- bi_class(parks_places, 
                      x = ASTHMA, 
                      y = PercentagePark, 
                      style = "quantile", dim = 3)

map2 <-   ggplot() +
  geom_sf(data = map2.data, 
          mapping = aes(fill = bi_class), 
          color = "grey", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal= "GrPink", dim = 3) + 
  theme_minimal()

# bivariate map 2 legend
map2.legend <- bi_legend(pal = "GrPink",
                         dim = 3,
                         xlab = "Asthma",
                         ylab = "Park Accessibility",
                         size = 5,
                         pad_width = 1) + 
  theme_minimal()

map2.bivariate <- map2 + map2.legend + plot_layout(widths = c(4,1))

ggsave(map2.bivariate,
       filename = "bivariate_maps/map2_bivariate.png",
       scale = 1,
       dpi = 300,
       height = 4,
       width = 8.5,
       units="in")


#--------------#


# -- Map 3 Bivariate
map3.data <- bi_class(parks_places, 
                      x = CANCER, 
                      y = PercentagePark, 
                      style = "quantile", dim = 3)

map3 <-   ggplot() +
  geom_sf(data = map3.data, 
          mapping = aes(fill = bi_class), 
          color = "grey", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal= "GrPink", dim = 3) + 
  theme_minimal()

# bivariate map 3 legend
map3.legend <- bi_legend(pal = "GrPink",
                         dim = 3,
                         xlab = "Cancer",
                         ylab = "Park Accessibility",
                         size = 5,
                         pad_width = 1) + 
  theme_minimal()

map3.bivariate <- map3 + map3.legend + plot_layout(widths = c(4,1))

ggsave(map3.bivariate,
       filename = "bivariate_maps/map3_bivariate.png",
       scale = 1,
       dpi = 300,
       height = 4,
       width = 8.5,
       units="in")


#--------------#


# -- Map 4 Bivariate
map4.data <- bi_class(parks_places, 
                      x = LPA, 
                      y = PercentagePark, 
                      style = "quantile", dim = 3)

map4 <-   ggplot() +
  geom_sf(data = map4.data, 
          mapping = aes(fill = bi_class), 
          color = "grey", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal= "GrPink", dim = 3) + 
  theme_minimal()

# bivariate map 4 legend
map4.legend <- bi_legend(pal = "GrPink",
                         dim = 3,
                         xlab = "Leisure Physical Activity",
                         ylab = "Park Accessibility",
                         size = 5,
                         pad_width = 1) + 
  theme_minimal()

map4.bivariate <- map4 + map4.legend + plot_layout(widths = c(4,1))

ggsave(map4.bivariate,
       filename = "bivariate_maps/map4_bivariate.png",
       scale = 1,
       dpi = 300,
       height = 4,
       width = 8.5,
       units="in")

#---------------------#

# -- Map 5 Bivariate
map5.data <- bi_class(parks_places, 
                      x = BPHIGH, 
                      y = PercentagePark, 
                      style = "quantile", dim = 3)

map5 <-   ggplot() +
  geom_sf(data = map5.data, 
          mapping = aes(fill = bi_class), 
          color = "grey", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal= "GrPink", dim = 3) + 
  theme_minimal()

# bivariate map 5 legend
map5.legend <- bi_legend(pal = "GrPink",
                         dim = 3,
                         xlab = "High Blood Pressure",
                         ylab = "Park Accessibility",
                         size = 5,
                         pad_width = 1) + 
  theme_minimal()

map5.bivariate <- map5 + map5.legend + plot_layout(widths = c(4,1))

ggsave(map5.bivariate,
       filename = "bivariate_maps/map5_bivariate.png",
       scale = 1,
       dpi = 300,
       height = 4,
       width = 8.5,
       units="in")

#---------------------------#

# -- Map 6 Bivariate
map6.data <- bi_class(parks_places, 
                      x = GHLTH, 
                      y = PercentagePark, 
                      style = "quantile", dim = 3)

map6 <-   ggplot() +
  geom_sf(data = map6.data, 
          mapping = aes(fill = bi_class), 
          color = "grey", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal= "GrPink", dim = 3) + 
  theme_minimal()

# bivariate map 6 legend
map6.legend <- bi_legend(pal = "GrPink",
                         dim = 3,
                         xlab = "Fair/Poor Health",
                         ylab = "Park Accessibility",
                         size = 5,
                         pad_width = 1) + 
  theme_minimal()

map6.bivariate <- map6 + map6.legend + plot_layout(widths = c(4,1))

ggsave(map6.bivariate,
       filename = "bivariate_maps/map6_bivariate.png",
       scale = 1,
       dpi = 300,
       height = 4,
       width = 8.5,
       units="in")



# NEW correlation maps - lack of park access ------------------------------



# correlation map 1 NEW - binge drinking ------------------


# -- Map 1 new Bivariate
map1new.data <- bi_class(parks_places, 
                      x = BINGE, 
                      y = NoParkAccess, 
                      style = "quantile", dim = 3)

map1new <-   ggplot() +
  geom_sf(data = map1new.data, 
          mapping = aes(fill = bi_class), 
          color = "grey", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal= "GrPink", dim = 3) + 
  theme_minimal()

# bivariate map 1 new legend
map1new.legend <- bi_legend(pal = "GrPink",
                         dim = 3,
                         xlab = "Binge Drinking",
                         ylab = "No Park Accessibility",
                         size = 5,
                         pad_width = 1) + 
  theme_minimal()

map1new.bivariate <- map1new + map1new.legend + plot_layout(widths = c(4,1))

ggsave(map1new.bivariate,
       filename = "bivariate_maps/map1new_bivariate.png",
       scale = 1,
       dpi = 300,
       height = 4,
       width = 8.5,
       units="in")

#--------------------------#


# -- Map 2 new Bivariate
map2new.data <- bi_class(parks_places, 
                      x = ASTHMA, 
                      y = NoParkAccess, 
                      style = "quantile", dim = 3)

map2new <-   ggplot() +
  geom_sf(data = map2new.data, 
          mapping = aes(fill = bi_class), 
          color = "grey", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal= "GrPink", dim = 3) + 
  theme_minimal()

# bivariate map 2 new legend
map2new.legend <- bi_legend(pal = "GrPink",
                         dim = 3,
                         xlab = "Asthma",
                         ylab = "No Park Accessibility",
                         size = 5,
                         pad_width = 1) + 
  theme_minimal()

map2new.bivariate <- map2new + map2new.legend + plot_layout(widths = c(4,1))

ggsave(map2new.bivariate,
       filename = "bivariate_maps/map2new_bivariate.png",
       scale = 1,
       dpi = 300,
       height = 4,
       width = 8.5,
       units="in")

#------------------------------#


# -- Map 3 new Bivariate
map3new.data <- bi_class(parks_places, 
                      x = CANCER, 
                      y = NoParkAccess, 
                      style = "quantile", dim = 3)

map3new <-   ggplot() +
  geom_sf(data = map3new.data, 
          mapping = aes(fill = bi_class), 
          color = "grey", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal= "GrPink", dim = 3) + 
  theme_minimal()

# bivariate map 3 new legend
map3new.legend <- bi_legend(pal = "GrPink",
                         dim = 3,
                         xlab = "Cancer",
                         ylab = "No Park Accessibility",
                         size = 5,
                         pad_width = 1) + 
  theme_minimal()

map3new.bivariate <- map3new + map3new.legend + plot_layout(widths = c(4,1))

ggsave(map3new.bivariate,
       filename = "bivariate_maps/map3new_bivariate.png",
       scale = 1,
       dpi = 300,
       height = 4,
       width = 8.5,
       units="in")

# -------------------------#

# -- Map 4 new Bivariate
map4new.data <- bi_class(parks_places, 
                      x = LPA, 
                      y = NoParkAccess, 
                      style = "quantile", dim = 3)

map4new <-   ggplot() +
  geom_sf(data = map4new.data, 
          mapping = aes(fill = bi_class), 
          color = "grey", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal= "GrPink", dim = 3) + 
  theme_minimal()

# bivariate map 4 new legend
map4new.legend <- bi_legend(pal = "GrPink",
                         dim = 3,
                         xlab = "Leisure Physical Activity",
                         ylab = "No Park Accessibility",
                         size = 5,
                         pad_width = 1) + 
  theme_minimal()

map4new.bivariate <- map4new + map4new.legend + plot_layout(widths = c(4,1))

ggsave(map4new.bivariate,
       filename = "bivariate_maps/map4new_bivariate.png",
       scale = 1,
       dpi = 300,
       height = 4,
       width = 8.5,
       units="in")

#-------------------------#

# -- Map 5 new Bivariate
map5new.data <- bi_class(parks_places, 
                      x = BPHIGH, 
                      y = NoParkAccess, 
                      style = "quantile", dim = 3)

map5new <-   ggplot() +
  geom_sf(data = map5new.data, 
          mapping = aes(fill = bi_class), 
          color = "grey", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal= "GrPink", dim = 3) + 
  theme_minimal()

# bivariate map 5 new legend
map5new.legend <- bi_legend(pal = "GrPink",
                         dim = 3,
                         xlab = "High Blood Pressure",
                         ylab = "No Park Accessibility",
                         size = 5,
                         pad_width = 1) + 
  theme_minimal()

map5new.bivariate <- map5new + map5new.legend + plot_layout(widths = c(4,1))

ggsave(map5new.bivariate,
       filename = "bivariate_maps/map5new_bivariate.png",
       scale = 1,
       dpi = 300,
       height = 4,
       width = 8.5,
       units="in")

#---------------------------#

# -- Map 6 NEW Bivariate
map6new.data <- bi_class(parks_places, 
                      x = GHLTH, 
                      y = NoParkAccess, 
                      style = "quantile", dim = 3)

map6new <-   ggplot() +
  geom_sf(data = map6new.data, 
          mapping = aes(fill = bi_class), 
          color = "grey", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal= "GrPink", dim = 3) + 
  theme_minimal()

# bivariate map 6 new legend
map6new.legend <- bi_legend(pal = "GrPink",
                         dim = 3,
                         xlab = "Fair/Poor Health",
                         ylab = "No Park Accessibility",
                         size = 5,
                         pad_width = 1) + 
  theme_minimal()

map6new.bivariate <- map6new + map6new.legend + plot_layout(widths = c(4,1))

ggsave(map6new.bivariate,
       filename = "bivariate_maps/map6new_bivariate.png",
       scale = 1,
       dpi = 300,
       height = 4,
       width = 8.5,
       units="in")


# correlation coefficient -------------------------------------------------

cor.test(parks_places$GHLTH, parks_places$NoParkAccess)


cor.test(parks_places$ASTHMA, parks_places$NoParkAccess)


cor.test(parks_places$BINGE, parks_places$NoParkAccess)


cor.test(parks_places$CANCER, parks_places$NoParkAccess)


cor.test(parks_places$BPHIGH, parks_places$NoParkAccess)


cor.test(parks_places$LPA, parks_places$NoParkAccess)
