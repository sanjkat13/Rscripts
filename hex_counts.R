# counts per hex
# distance to inst by reg div
# 8/28/2023


# pckg --------------------------------------------------------------------

library(dplyr) # data wrangling with pipe syntax
library(tidyverse) # data wrangling
library(sf) # simple features data package
library(sp) # spatial data package
library(openxlsx) # use for importing data in Excel format
library(units) # manage measurement units
library(ggplot2)


# files -------------------------------------------------------------------


regDiv <- read_sf("C://Users//skatw//Documents//GIS//AAG_regional_divisions//shapefiles//studyarea_divboundaries.shp") %>% 
  select(REGDIVABBR) %>% 
  st_transform(crs = 4326)
#sp::plot(regDiv)

studyArea <- read_sf("C://Users//skatw//Documents//GIS//AAG_regional_divisions//shapefiles//GuideData_StudyArea_824.shp") %>% 
  select(School,
         Department,
         CoursesOnl,
         Minor,
         Concentrat,
         Certificat,
         Associates,
         Bachelors,
         Masters,
         Doctorate) %>%
  group_by(School) %>% 
  mutate(Department = str_c(Department, collapse = "//")) %>% 
  distinct() %>% 
  st_transform(crs = 4326)

#sp:: plot(studyArea)

# join regDiv to study area spreadsheet by intersection
studyArea_regDiv <- st_join(studyArea, regDiv,
                      join=st_intersects) %>% 
  st_as_sf() 
#sp::plot(studyArea_regDiv %>% select(REGDIVABBR))


hex <- regDiv %>%  
  st_make_grid(cellsize = 0.7, square = FALSE) %>%
  st_transform(crs=4326) %>%
  st_cast("POLYGON") %>%
  st_centroid() %>%
  st_as_sf() %>%
  mutate(hexid = row_number()) %>%
  st_intersection(regDiv) %>%
  select(hexid,geometry=x, REGDIVABBR) %>% 
  st_as_sf()
hex <- hex %>%
  bind_cols(st_coordinates(hex)) %>%
  rename(lat = Y,
         lon = X)
sp::plot(hex)

# calculating distance


# calc distance between hex and institutions
distance <- st_distance(hex, studyArea) %>% 
  as.data.frame() %>%
  drop_units() %>%
  mutate_if(is.numeric, ~ . * 0.000621371) %>% #in miles
  bind_cols(hex) %>% 
  select('hexid','lat', 'lon', V1:V965)

matrix_distance_sum <- distance %>%
  rowwise() %>%
  mutate(min = min(across(V1:V965)),### find the minimum for each column (closest past meeting loc for each inst)
         max = max(across(V1:V965)),
         mean = mean(V1:V965),##avg distance to all past meeting locs
  ) %>% 
  select('hexid', 'min','max','mean')

# join distance matrix table to hex shapefile
hex_sum <- left_join(hex, matrix_distance_sum, by="hexid")

sp::plot(hex_sum %>% select('min'))

write_sf(hex_sum,"C://Users//skatw//Documents//GIS//AAG_regional_divisions//shapefiles//hex_distance_sum.shp" )

## calculate it grouped by regDiv -----------------------------------

# get distance by reg div abbr
# list of abbreviations: 
#APCG, NESTVAL, ELDAAG, GPRM,MAD,MSAAG,SEDAAG,SWAAG,WLDAAG                                                                                                                                   13

# APCG --------------------

hex_APCG <- hex %>% 
  filter(REGDIVABBR=='APCG') 
APCG <- studyArea_regDiv %>% 
  filter(REGDIVABBR=='APCG')
# calc distance
APCG_distance <- st_distance(hex_APCG, APCG) %>% 
  as.data.frame() %>%
  drop_units() %>%
  mutate_if(is.numeric, ~ . * 0.000621371) %>% 
  bind_cols(hex_APCG)
# summary
APCG_matrix<- APCG_distance %>% 
  rowwise() %>%
  mutate(min = min(across(V1:V224)),### find the minimum for each column (closest past meeting loc for each inst)
         max = max(across(V1:V224)),
         mean = mean(V1:V224),
  ) %>% 
  select('hexid','min','max','mean')
# join
APCG_distance_sf <- left_join(hex_APCG, APCG_matrix, by="hexid")
# plot
sp::plot(APCG_distance_sf %>% select(min))

# NESTVAL -------------------

hex_NESTVAL <- hex %>% 
  filter(REGDIVABBR=='NESTVAL') 
NESTVAL <- studyArea_regDiv %>% 
  filter(REGDIVABBR=='NESTVAL')
# calc distance
NESTVAL_distance <- st_distance(hex_NESTVAL, NESTVAL) %>% 
  as.data.frame() %>%
  drop_units() %>%
  mutate_if(is.numeric, ~ . * 0.000621371) %>% 
  bind_cols(hex_NESTVAL)
# summary
NESTVAL_matrix<- NESTVAL_distance %>% 
  rowwise() %>%
  mutate(min = min(across(V1:V59)),### find the minimum for each column (closest past meeting loc for each inst)
         max = max(across(V1:V59)),
         mean = mean(V1:V59),
  ) %>% 
  select('hexid','min','max','mean')
# join
NESTVAL_distance_sf <- left_join(hex_NESTVAL, NESTVAL_matrix, by="hexid")
# plot
sp::plot(NESTVAL_distance_sf %>% select(min))

# ELDAAG -------------------

hex_ELDAAG <- hex %>% 
  filter(REGDIVABBR=='ELDAAG') 
ELDAAG <- studyArea_regDiv %>% 
  filter(REGDIVABBR=='ELDAAG')
# calc distance
ELDAAG_distance <- st_distance(hex_ELDAAG, ELDAAG) %>% 
  as.data.frame() %>%
  drop_units() %>%
  mutate_if(is.numeric, ~ . * 0.000621371) %>% 
  bind_cols(hex_ELDAAG)
# summary
ELDAAG_matrix<- ELDAAG_distance %>% 
  rowwise() %>%
  mutate(min = min(across(V1:V55)),### find the minimum for each column (closest past meeting loc for each inst)
         max = max(across(V1:V55)),
         mean = mean(V1:V55),
  ) %>% 
  select('hexid','min','max','mean')
# join
ELDAAG_distance_sf <- left_join(hex_ELDAAG, ELDAAG_matrix, by="hexid")
# plot
sp::plot(ELDAAG_distance_sf %>% select(min))


# GPRM --------------------

hex_GPRM <- hex %>% 
  filter(REGDIVABBR=='GPRM') 
GPRM <- studyArea_regDiv %>% 
  filter(REGDIVABBR=='GPRM')
# calc distance
GPRM_distance <- st_distance(hex_GPRM, GPRM) %>% 
  as.data.frame() %>%
  drop_units() %>%
  mutate_if(is.numeric, ~ . * 0.000621371) %>% 
  bind_cols(hex_GPRM)
# summary
GPRM_matrix<- GPRM_distance %>% 
  rowwise() %>%
  mutate(min = min(across(V1:V80)),### find the minimum for each column (closest past meeting loc for each inst)
         max = max(across(V1:V80)),
         mean = mean(V1:V80),
  ) %>% 
  select('hexid','min','max','mean')
# join
GPRM_distance_sf <- left_join(hex_GPRM, GPRM_matrix, by="hexid")
# plot
sp::plot(GPRM_distance_sf %>% select(min))

# MAD --------------------

hex_MAD <- hex %>% 
  filter(REGDIVABBR=='MAD') 
MAD <- studyArea_regDiv %>% 
  filter(REGDIVABBR=='MAD')
# calc distance
MAD_distance <- st_distance(hex_MAD, MAD) %>% 
  as.data.frame() %>%
  drop_units() %>%
  mutate_if(is.numeric, ~ . * 0.000621371) %>% 
  bind_cols(hex_MAD)
# summary
MAD_matrix<- MAD_distance %>% 
  rowwise() %>%
  mutate(min = min(across(V1:V19)),### find the minimum for each column (closest past meeting loc for each inst)
         max = max(across(V1:V19)),
         mean = mean(V1:V19),
  ) %>% 
  select('hexid','min','max','mean')
# join
MAD_distance_sf <- left_join(hex_MAD, MAD_matrix, by="hexid")
# plot
sp::plot(MAD_distance_sf %>% select(min))

# MSAAG --------------------

hex_MSAAG <- hex %>% 
  filter(REGDIVABBR=='MSAAG') 
MSAAG <- studyArea_regDiv %>% 
  filter(REGDIVABBR=='MSAAG')
# calc distance
MSAAG_distance <- st_distance(hex_MSAAG, MSAAG) %>% 
  as.data.frame() %>%
  drop_units() %>%
  mutate_if(is.numeric, ~ . * 0.000621371) %>% 
  bind_cols(hex_MSAAG)
# summary
MSAAG_matrix<- MSAAG_distance %>% 
  rowwise() %>%
  mutate(min = min(across(V1:V99)),### find the minimum for each column (closest past meeting loc for each inst)
         max = max(across(V1:V99)),
         mean = mean(V1:V99),
  ) %>% 
  select('hexid','min','max','mean')
# join
MSAAG_distance_sf <- left_join(hex_MSAAG, MSAAG_matrix, by="hexid")
# plot
sp::plot(MSAAG_distance_sf %>% select(min))

# SEDAAG ---------------------

hex_SEDAAG <- hex %>% 
  filter(REGDIVABBR=='SEDAAG') 
SEDAAG <- studyArea_regDiv %>% 
  filter(REGDIVABBR=='SEDAAG')
# calc distance
SEDAAG_distance <- st_distance(hex_SEDAAG, SEDAAG) %>% 
  as.data.frame() %>%
  drop_units() %>%
  mutate_if(is.numeric, ~ . * 0.000621371) %>% 
  bind_cols(hex_SEDAAG)
# summary
SEDAAG_matrix<- SEDAAG_distance %>% 
  rowwise() %>%
  mutate(min = min(across(V1:V155)),### find the minimum for each column (closest past meeting loc for each inst)
         max = max(across(V1:V155)),
         mean = mean(V1:V155),
  ) %>% 
  select('hexid','min','max','mean')
# join
SEDAAG_distance_sf <- left_join(hex_SEDAAG, SEDAAG_matrix, by="hexid")
# plot
sp::plot(SEDAAG_distance_sf %>% select(min))

# SWAAG ---------------------

hex_SWAAG <- hex %>% 
  filter(REGDIVABBR=='SWAAG') 
SWAAG <- studyArea_regDiv %>% 
  filter(REGDIVABBR=='SWAAG')
# calc distance
SWAAG_distance <- st_distance(hex_SWAAG, SWAAG) %>% 
  as.data.frame() %>%
  drop_units() %>%
  mutate_if(is.numeric, ~ . * 0.000621371) %>% 
  bind_cols(hex_SWAAG)
# summary
SWAAG_matrix<- SWAAG_distance %>% 
  rowwise() %>%
  mutate(min = min(across(V1:V106)),### find the minimum for each column (closest past meeting loc for each inst)
         max = max(across(V1:V106)),
         mean = mean(V1:V106),
  ) %>% 
  select('hexid','min','max','mean')
# join
SWAAG_distance_sf <- left_join(hex_SWAAG, SWAAG_matrix, by="hexid")
# plot
sp::plot(SWAAG_distance_sf %>% select(min))

# WLDAAG --------------------

hex_WLDAAG <- hex %>% 
  filter(REGDIVABBR=='WLDAAG') 
WLDAAG <- studyArea_regDiv %>% 
  filter(REGDIVABBR=='WLDAAG')
# calc distance
WLDAAG_distance <- st_distance(hex_WLDAAG, WLDAAG) %>% 
  as.data.frame() %>%
  drop_units() %>%
  mutate_if(is.numeric, ~ . * 0.000621371) %>% 
  bind_cols(hex_WLDAAG)
# summary
WLDAAG_matrix<- WLDAAG_distance %>% 
  rowwise() %>%
  mutate(min = min(across(V1:V135)),### find the minimum for each column (closest past meeting loc for each inst)
         max = max(across(V1:V135)),
         mean = mean(V1:V135),
  ) %>% 
  select('hexid','min','max','mean')
# join
WLDAAG_distance_sf <- left_join(hex_WLDAAG, WLDAAG_matrix, by="hexid")
# plot
sp::plot(WLDAAG_distance_sf %>% select(min))


## write all into shapefiles
# APCG, NESTVAL, ELDAAG, GPRM,MAD,MSAAG,SEDAAG,SWAAG,WLDAAG 

write_sf(APCG_distance_sf,
         "C://Users//skatw//Documents//GIS//AAG_regional_divisions//shapefiles//regDivHex//APCG_distance_sf.shp" )

write_sf(NESTVAL_distance_sf,
         "C://Users//skatw//Documents//GIS//AAG_regional_divisions//shapefiles//regDivHex//NESTVAL_distance_sf.shp" )

write_sf(ELDAAG_distance_sf,
         "C://Users//skatw//Documents//GIS//AAG_regional_divisions//shapefiles//regDivHex//ELDAAG_distance_sf.shp" )

write_sf(GPRM_distance_sf,
         "C://Users//skatw//Documents//GIS//AAG_regional_divisions//shapefiles//regDivHex//GPRM_distance_sf.shp" )

write_sf(MAD_distance_sf,
         "C://Users//skatw//Documents//GIS//AAG_regional_divisions//shapefiles//regDivHex//MAD_distance_sf.shp" )

write_sf(MSAAG_distance_sf,
         "C://Users//skatw//Documents//GIS//AAG_regional_divisions//shapefiles//regDivHex//MSAAG_distance_sf.shp" )

write_sf(SEDAAG_distance_sf,
         "C://Users//skatw//Documents//GIS//AAG_regional_divisions//shapefiles//regDivHex//SEDAAG_distance_sf.shp" )

write_sf(SWAAG_distance_sf,
         "C://Users//skatw//Documents//GIS//AAG_regional_divisions//shapefiles//regDivHex//SWAAG_distance_sf.shp" )

write_sf(WLDAAG_distance_sf,
         "C://Users//skatw//Documents//GIS//AAG_regional_divisions//shapefiles//regDivHex//WLDAAG_distance_sf.shp" )
