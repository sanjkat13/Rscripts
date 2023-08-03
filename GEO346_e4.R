library(dplyr) # data wrangling with pipe syntax
library(tidyverse) # data wrangling
library(sf) # simple features data package
library(sp) # spatial data package
library(openxlsx) # use for importing data in Excel format

options(scipen=999, digits = 2) # format output for data tables

#HUC4 - 0406 (NW michigan)

# TRI data ----------------------------------------------------------------

# Toxics Release Inventory 2021 (csv format)
TRI_2021_raw <- read_csv("https://enviro.epa.gov/enviro/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2021/fname/TRI_2021_US.csv/CSV?.csv")

# format TRI data
# filter data water releases in California
TRI_2021_form <- TRI_2021_raw %>%
  rename(
    year = 1,          #rename columns so theyre easier to work with
    frs_id = 3,
    fac_name = 4,
    fac_street = 5,
    fac_city = 6,
    fac_county = 7,
    fac_state = 8,
    fac_zip = 9,
    latitude = 12,
    longitude = 13,
    parent_co = 15,
    sector_code = 19,
    sector_name = 20,
    chemical = 34,
    casno = 37,
    srsid = 38,
    caa_chemical = 39,
    carcinogen = 43,
    pfas = 44,
    emissions_uom = 46,
    fugitive_air = 47,
    stack_air = 48,
    water = 49,
    underground = 50
  ) %>%
  select(
    year,
    frs_id,
    fac_name,
    fac_street,
    fac_city,
    fac_county,
    fac_state,
    fac_zip,
    latitude,
    longitude,
    parent_co,
    sector_code,
    sector_name,
    chemical,
    casno,
    srsid,
    caa_chemical,
    carcinogen,
    pfas,
    emissions_uom,
    fugitive_air,
    stack_air,
    water,
    underground
  ) %>%
  filter(water > 0) #remove facilities that did not report water releases


# toxicity weights to be able to compare ----------------------------------

# Import RSEI data (Excel format) for weighting toxicity of water emissions
RSEI_v2310_raw <- read.xlsx("https://www.epa.gov/system/files/other-files/2022-02/toxicity_data_rsei_v2310.xlsx", sheet=2)

RSEI_v2310_form <- RSEI_v2310_raw %>%
  rename(casno = CASStandard) %>%
  mutate(itw = as.numeric(ITW)) %>%
  select(casno,
         itw)

# Weight TRI data using RSEI data and summarize by facility
TRI_2021_form_wtd <- TRI_2021_form %>%
  drop_na(latitude, longitude) %>%
  left_join(RSEI_v2310_form, by = "casno") %>%
  mutate(water_itw = water * itw/10000000) %>%
  drop_na(water_itw) %>%
  select(-itw) %>%
  group_by(frs_id) %>%
  summarise(water_itw_sum = sum(water_itw)) %>%
  distinct()

# add facility name to TRI data
TRI_2021_form_wtd_xy <-  TRI_2021_form_wtd %>% left_join(TRI_2021_form %>% select(frs_id, fac_name, latitude, longitude) %>% drop_na(latitude, longitude) %>% distinct(), by="frs_id") %>%
  select(-frs_id)

# create shapefile from weighted TRI emissions table
TRI_2021_form_wtd_geom <- st_as_sf(TRI_2021_form_wtd_xy, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_as_sf()

# write TRI data to layers folder in project directory
st_write(TRI_2021_form_wtd_geom, "TRI_WaterReleases_2021.shp", append=FALSE)
