### 

# install packages
library(censusapi) # retrieving census attribute data (population, housing) 
library(tigris) # retrieving census geometries
library(sf) # manipulating geometry data #simple feature package
library(leaflet) # making interactive maps
library(dplyr)
library(tidyverse)
library(ggplot2)
library(biscale)
library(corrplot)
library(scales)
library(clipr)
library(patchwork)
library(RColorBrewer)

#download all US counties by state with tigris package
us_counties_geom <- counties(cb=TRUE, class="sf", year = 2020) %>%
  mutate(STATEFP_NO = as.numeric(STATEFP)) %>%
  filter(STATEFP_NO <= 56, STATEFP_NO != 15, STATEFP_NO != 2) 

# plot US by boundaries
#plot(us_counties_geom['COUNTYFP']) #plots each county individually
plot(us_counties_geom['STATEFP']) # inside brackets is the column name

#download 2020 census data with censusapi

censusapikey <- "0bad56e1a374c753569328e2535d99d30eb29ead" #mine
#variables for function
agroup <- c("B01001")
varlist <- c("B01001_001E")
yearlist <- c(2020)

for (ayear in yearlist) {
  agroupname = paste("group(",agroup,")",sep="")
  acs_group <- getCensus(name = "acs/acs5",
                         vintage = ayear,
                         vars = c("NAME",varlist),
                         region = "county:*",
                         regionin="state:*",
                         key=censusapikey)
  attach(acs_group)
 
  acs_group <- acs_group %>% select(-contains(c("EA",
                                                "MA",
                                                "GEO_ID",
                                                "M_1")))
  acs_group$year<-ayear 
  acs_group$GEOID<-paste0(state,county)
  assign(paste(agroup,ayear,sep="_"),acs_group)
  rm(acs_group)
  detach(acs_group)
}

# Join population data to county geometries
us_counties_population_geo <- us_counties_geom %>%
  select(NAME,
         STATE_NAME,
         GEOID) %>%
  left_join(B01001_2020 %>% 
              select("GEOID",
                     pop_2020 = B01001_001E),
            by="GEOID")  %>%
  mutate(pop_2020_q = ntile(pop_2020, 5)) %>%
  st_as_sf()

# Summary table of counties by population quintile category
countiestable <- us_counties_population_geo %>% 
  st_drop_geometry() %>%
  group_by(pop_2020_q) %>%
  summarise(count = n(),
            min_pop = min(pop_2020),
            max_pop = max(pop_2020),
            avg_pop = mean(pop_2020),
            sd_pop = sd(pop_2020))

# view table of counties over 1 million pop
view(us_counties_population_geo %>% 
       select(-pop_2020_q) %>%
       st_drop_geometry() %>% 
       filter(pop_2020 >= 1000000) %>% 
       arrange(-pop_2020))


#counties_bins <- c(0, 8958, 18945, 36947, 999999, Inf) # loosely based on quintile breaks



county_fips = "04019" # FIPS code for Pima Arizona (pop: 1,038,476)
crs_code = 4326 # coordinate reference system

# Download track boundaries for 04019
tract_boundaries_geom <- tracts(state =  substr(county_fips,1,2), 
                                county = substr(county_fips,3,5), 
                                cb=TRUE, 
                                class="sf", 
                                year = 2010) %>%
  mutate(SQMI = CENSUSAREA,
         GEOID = paste0(STATE,COUNTY,TRACT)) %>%
  select(GEOID, SQMI) %>%
  st_as_sf()
# plot map of county and tracts
plot(tract_boundaries_geom['SQMI'], main="Pima County, Arizona")

#write_csv(tract_boundaries_geom,"C:/GIS/GEO346/tract_boundaries_geom_pima.csv")

# PLACES data
#places_2021_tracts_all <- read_csv("https://chronicdata.cdc.gov/api/views/cwsq-ngmh/rows.csv?accessType=DOWNLOAD&bom=true&format=true")

places_2021_tracts_all <- read_csv("C:/GIS/GEO346/places_2021_tracts_all.csv")
#write.csv(places_2021_tracts_all, "C:/GIS/GEO346/places_2021_tracts_all.csv")

#summary of each places data measures 
places_summary <- places_2021_tracts_all %>% #places_data_dictionary_summary
  filter(CountyFIPS==county_fips) %>% #filter by Pima county FIPs code
  group_by(Category, Measure, MeasureId) %>%
  summarise(mean = mean(Data_Value),  #summarize data
            min = min(Data_Value), 
            max = max(Data_Value), 
            sd = sd(Data_Value))

# list of measures 
places_measures_list <- c("CHECKUP","ACCESS2","DEPRESSION","TEETHLOST")

# create summary data table for PLACES measures
places_data_dictionary_summary_sel <- places_summary %>%
  filter(MeasureId %in% places_measures_list)

write_clip(places_data_dictionary_summary_sel)

# create detailed data table of selected PLACES measures in wide format
places_2021_tracts_sel <- places_2021_tracts_all %>%
  filter(CountyFIPS==county_fips, MeasureId %in% places_measures_list) %>% 
  #includes tracts only in your county and only the measures in your list
  select(GEOID = LocationName,
         MeasureId,
         Data_Value,
         Population = TotalPopulation) %>%
  pivot_wider(names_from=MeasureId, 
              values_from = Data_Value)

#rm(places_2021_tracts_all) #remove big files 

# perform attribute join detailed data table to census geometries mapping of PLACES measures
places_geom <- places_2021_tracts_sel %>%  #places_2021_tracts_sel_geom
  left_join(tract_boundaries_geom, by = "GEOID") %>%
  mutate(Population_density = Population/SQMI) %>%
  st_as_sf()

# Import Social Vulnerability Index (SVI) data from CDC --------
#svi_2018_tracts_all <- read_csv("https://data.cdc.gov/api/views/4d8n-kk8a/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
#write_csv(svi_2018_tracts_all,"C:/GIS/GEO346/svi_2018_tracts_all.csv")
svi_2018_tracts_all <- read_csv("C:/GIS/GEO346/svi_2018_tracts_all.csv")

# Import data dictionary from course GitHub
svi_data_dicionary <- read_csv("https://raw.githubusercontent.com/justenvirons/pedagogy/main/GEO346_2022_FallQuarter/Exercise_01/data/svi_2018_datadictionary.csv") %>%
  filter(str_detect(Name, "EP_|RPL_")) %>%
  arrange(Theme)

# four SVI variable names in this list with those you'd like to evaluate
svi_factors_list <- c("EP_MINRTY","EP_DISABL","EP_PCI","EP_LIMENG")


# create detailed data table of selected SVI factors 
svi_2018_tracts_sel <- svi_2018_tracts_all %>%
  filter(substr(FIPS,1,5)==county_fips) %>%
  select(GEOID = FIPS,
         contains(svi_factors_list)) %>%
  mutate(across(where(is.numeric), ~na_if(., -999))) %>%
  mutate_at(svi_factors_list, function(x) rescale(x,to = c(0,100)))

# Create comprehensive table containing both PLACES and SVI data ----------
svi_geom <- svi_2018_tracts_sel %>% #svi_places_tracts_sel_geom
  left_join(places_geom, by = "GEOID") %>%
  st_as_sf() 

# Create bivariate correlation plot of Places x SVI ---------------------------------------

svi_places_tracts_sel_sub <- svi_geom %>%
  st_drop_geometry() %>%
  drop_na() %>%
  select(-c(GEOID,Population,SQMI,Population_density))

variable_biv_correlations <- cor(svi_places_tracts_sel_sub)

corrplot(variable_biv_correlations, method="number", col=c("black","white"), order="hclust", bg="lightgrey", tl.col = "grey")
corrplot(variable_biv_correlations, col=c("black","white"), order="hclust", bg="lightgrey", tl.col = "grey")

# ------ Maps --------------------------------------------------

# Maps 1 - Depression x Disability
# -- Map 1 Depression
map1.Depression.Places <- ggplot() + geom_sf(data=svi_geom,
                                  mapping = aes(fill = ntile(DEPRESSION, 5)),
                                  size = 0.1) + 
  scale_fill_distiller(name="PLACES", 
                       palette = "Greys", 
                       breaks = pretty_breaks(),
                       direction = 1) + 
  theme_minimal()

ggsave(map1.Depression.Places,
       filename = "C:/GIS/GEO346/map1_Depression_Places.png",
       scale = 1,
       dpi = 300,
       width = 5.5,
       height = 5.5,
       units="in")
# -- Map 1 SVI
map1.Disabled.SVI <- ggplot() + geom_sf(data=svi_geom,
                               mapping = aes(fill = ntile(EP_DISABL, 5)),
                               size = 0.1) + 
  scale_fill_distiller(name="SVI", 
                       palette = "Greys", 
                       breaks = pretty_breaks(n=4),
                       direction = 1) + 
  theme_minimal()

ggsave(map1.Disabled.SVI,
       filename = "C:/GIS/GEO346/map1_Disabled_svi.png",
       scale = 1,
       dpi = 300,
       width = 5.5,
       height = 5.5,
       units="in")

# -- Map 1 Bivariate
map1.data <- bi_class(svi_geom, 
                      x = DEPRESSION, 
                      y = EP_DISABL, 
                      style = "quantile", dim = 3)

map1 <-   ggplot() +
  geom_sf(data = map1.data, 
          mapping = aes(fill = bi_class), 
          color = "grey", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal = "BlueGold", dim = 3) + 
  theme_minimal()

# bivariate map 1 legend
map1.legend <- bi_legend(pal = "BlueGold",
                         dim = 3,
                         xlab = "Depression",
                         ylab = "Disability",
                         size = 5,
                         pad_width = 1) + 
  theme_minimal()

map1.bivariate <- map1 + map1.legend + plot_layout(widths = c(4,1))

ggsave(map1.bivariate,
       filename = "C:/GIS/GEO346/map1_bivariate.png",
       scale = 1,
       dpi = 300,
       height = 4,
       width = 8.5,
       units="in")


# ------------------------------------
# Maps 2 - Check ups x Perc Minority
# -- Map 2 Check ups
map2.checkups.Places <- ggplot() + geom_sf(data=svi_geom,
                                             mapping = aes(fill = ntile(CHECKUP, 5)),
                                             size = 0.1) + 
  scale_fill_distiller(name="PLACES", 
                       palette = "Greys", 
                       breaks = pretty_breaks(),
                       direction = 1) + 
  theme_minimal()

ggsave(map2.checkups.Places,
       filename = "C:/GIS/GEO346/map2_checkups_Places.png",
       scale = 1,
       dpi = 300,
       width = 5.5,
       height = 5.5,
       units="in")
# -- Map 2 SVI
map2.minority.SVI <- ggplot() + geom_sf(data=svi_geom,
                                        mapping = aes(fill = ntile(EP_MINRTY, 5)),
                                        size = 0.1) + 
  scale_fill_distiller(name="SVI", 
                       palette = "Greys", 
                       breaks = pretty_breaks(n=4),
                       direction = 1) + 
  theme_minimal()

ggsave(map2.minority.SVI,
       filename = "C:/GIS/GEO346/map2_minority_svi.png",
       scale = 1,
       dpi = 300,
       width = 5.5,
       height = 5.5,
       units="in")


# -- Map 2 Bivariate
map2.data <- bi_class(svi_geom, 
                      x = CHECKUP, 
                      y = EP_MINRTY, 
                      style = "quantile", dim = 3)

map2 <-   ggplot() +
  geom_sf(data = map2.data, 
          mapping = aes(fill = bi_class), 
          color = "grey", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal = "BlueGold", dim = 3) + 
  theme_minimal()

# bivariate map 2 legend
map2.legend <- bi_legend(pal = "BlueGold",
                         dim = 3,
                         xlab = "Check Ups",
                         ylab = "% Minority",
                         size = 5,
                         pad_width = 1) + 
  theme_minimal()

map2.bivariate <- map2 + map2.legend + plot_layout(widths = c(4,1))

ggsave(map2.bivariate,
       filename = "C:/GIS/GEO346/map2_bivariate.png",
       scale = 1,
       dpi = 300,
       height = 4,
       width = 8.5,
       units="in")

# -------------------------------
# Maps 3 - Teeth lost x PCI

# -- Map 3 Teeth Lost
map3.teeth.Places <- ggplot() + geom_sf(data=svi_geom,
                                           mapping = aes(fill = ntile(TEETHLOST, 5)),
                                           size = 0.1) + 
  scale_fill_distiller(name="PLACES", 
                       palette = "Greys", 
                       breaks = pretty_breaks(),
                       direction = 1) + 
  theme_minimal()

ggsave(map3.teeth.Places,
       filename = "C:/GIS/GEO346/map3_teeth_Places.png",
       scale = 1,
       dpi = 300,
       width = 5.5,
       height = 5.5,
       units="in")

# -- Map 3 SVI PCI
map3.pci.SVI <- ggplot() + geom_sf(data=svi_geom,
                                        mapping = aes(fill = ntile(EP_PCI, 5)),
                                        size = 0.1) + 
  scale_fill_distiller(name="SVI", 
                       palette = "Greys", 
                       breaks = pretty_breaks(n=4),
                       direction = 1) + 
  theme_minimal()

ggsave(map3.pci.SVI,
       filename = "C:/GIS/GEO346/map3_pci_svi.png",
       scale = 1,
       dpi = 300,
       width = 5.5,
       height = 5.5,
       units="in")


# -- Map 3 Bivariate
map3.data <- bi_class(svi_geom, 
                      x = TEETHLOST, 
                      y = EP_PCI, 
                      style = "quantile", dim = 3)

map3 <-   ggplot() +
  geom_sf(data = map3.data, 
          mapping = aes(fill = bi_class), 
          color = "grey", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal = "BlueGold", dim = 3) + 
  theme_minimal()

# bivariate map 3 legend
map3.legend <- bi_legend(pal = "BlueGold",
                         dim = 3,
                         xlab = "Teeth Lost",
                         ylab = "Per Capita Income",
                         size = 5,
                         pad_width = 1) + 
  theme_minimal()

map3.bivariate <- map3 + map3.legend + plot_layout(widths = c(4,1))

ggsave(map3.bivariate,
       filename = "C:/GIS/GEO346/map3_bivariate.png",
       scale = 1,
       dpi = 300,
       height = 4,
       width = 8.5,
       units="in")

# Maps 4 - Lack of Health Insurance x English less than well
# -- Map 4 Insurance
map4.insurance.Places <- ggplot() + geom_sf(data=svi_geom,
                                        mapping = aes(fill = ntile(ACCESS2, 5)),
                                        size = 0.1) + 
  scale_fill_distiller(name="PLACES", 
                       palette = "Greys", 
                       breaks = pretty_breaks(),
                       direction = 1) + 
  theme_minimal()

ggsave(map4.insurance.Places,
       filename = "C:/GIS/GEO346/map4_insurance_Places.png",
       scale = 1,
       dpi = 300,
       width = 5.5,
       height = 5.5,
       units="in")

# -- Map 4 SVI - English
map4.english.SVI <- ggplot() + geom_sf(data=svi_geom,
                                   mapping = aes(fill = ntile(EP_LIMENG, 5)),
                                   size = 0.1) + 
  scale_fill_distiller(name="SVI", 
                       palette = "Greys", 
                       breaks = pretty_breaks(n=4),
                       direction = 1) + 
  theme_minimal()

ggsave(map4.english.SVI,
       filename = "C:/GIS/GEO346/map4_english_svi.png",
       scale = 1,
       dpi = 300,
       width = 5.5,
       height = 5.5,
       units="in")


# -- Map 4 Bivariate
map4.data <- bi_class(svi_geom, 
                      x = ACCESS2, 
                      y = EP_LIMENG, 
                      style = "quantile", dim = 3)

map4 <-   ggplot() +
  geom_sf(data = map4.data, 
          mapping = aes(fill = bi_class), 
          color = "grey", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal = "BlueGold", dim = 3) + 
  theme_minimal()

# bivariate map 4 legend
map4.legend <- bi_legend(pal = "BlueGold",
                         dim = 3,
                         xlab = "no Health Insurance",
                         ylab = "Speaks English 'less than well'",
                         size = 5,
                         pad_width = 1) + 
  theme_minimal()

map4.bivariate <- map4 + map4.legend + plot_layout(widths = c(4,1))

ggsave(map4.bivariate,
       filename = "C:/GIS/GEO346/map4_bivariate.png",
       scale = 1,
       dpi = 300,
       height = 4,
       width = 8.5,
       units="in")