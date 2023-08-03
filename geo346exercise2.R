# packages ----------------------------------------------------------------

# activate packages
library(dplyr)
library(tidyverse)
library(openxlsx) # allows to read and write excel files
library(sf) # simple features data package - geometry
library(sp) # create spatial data from coordinates
# Ctrl Shift R -> creates a section label


# Download pollution data -------------------------------------------------

# National Emissions Territory
# download unzip & format pollution data

temp <- tempfile()
download.file("https://gaftp.epa.gov/air/nei/2017/data_summaries/2017v1/2017neiJan_facility.zip",temp)
NEI_2017_raw <- read_csv(unz(temp, "emis_sum_fac_15420.csv"))
unlink(temp)

NEI_2017_form <- NEI_2017_raw %>%
  rename(
    fac_name = `company name`,
    site_name = `site name`,
    fac_street = address,
    fac_city = city,
    fac_state = `postal abbreviation`,
    fac_zip = `zip code`,
    naics_code = `naics code`,
    naics_desc = `naics description`,
    latitude = `site latitude`,
    longitude = `site longitude`,
    pollutant_code = `pollutant code`,
    pollutant_desc = `pollutant desc`,
    pollutant_type = `pollutant type(s)`, #we're only gonna use sulfer dioxide
    emissions_uom = `emissions uom`,#unit of measurement
    emissions = `total emissions`#quantity released by facility
  ) %>%
  select(
    fac_name,
    site_name,
    fac_street,
    fac_city,
    fac_state,
    fac_zip,
    naics_code,
    naics_desc,
    latitude,
    longitude,
    pollutant_code,
    pollutant_desc,
    pollutant_type,
    emissions_uom,
    emissions
  ) %>%
  filter(fac_state == "CA",
         pollutant_code == "SO2")


# Toxics Release Inventory (csv format)
TRI_2020_raw <- read_csv("https://enviro.epa.gov/enviro/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2020/fname/TRI_2020_US.csv/CSV?.csv")

# format TRI data
# filter data stack air releases in California
TRI_2020_form <- TRI_2020_raw %>%
  rename(
    year = 1,
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
  filter(fac_state == "CA",
         stack_air > 0) # only those with release over 0

# Risk Screening Env Index - Toxicity weight data
# RSEI data (Excel format)
RSEI_v2310_raw <- read.xlsx("https://www.epa.gov/system/files/other-files/2022-02/toxicity_data_rsei_v2310.xlsx", sheet=2)

RSEI_v2310_form <- RSEI_v2310_raw %>%
  rename(casno = CASStandard) %>%
  mutate(itw = as.numeric(ITW)) %>%
  select(casno,
         itw)

remove(RSEI_v2310_raw)


# join TRI w ITWs ---------------------------------------------------------
# Join TRI with RSEI inhalation toxicity weights (ITW)
  # left join by column (chemical ID),
  #multiply stack air emossions by ITW by chemical,
  #divide by 10,000 to decrease the large numbers bc aermods doesn't like it
    #some chems dont have TWs which is a limitation of the data

TRI_2020_form_wtd <- TRI_2020_form %>%
  left_join(RSEI_v2310_form, by = "casno") %>%
  mutate(stack_air_itw = stack_air * itw/10000000) %>%
  drop_na(stack_air_itw) %>%
  select(-itw)


# write Shapefiles to proj directory --------------------------------------


TRI_2020_form_wtd_geom <- st_as_sf(TRI_2020_form_wtd, coords = c("longitude", "latitude"), crs = 4269) %>% st_transform(crs=26911) %>%
  st_as_sf() # or crs = 26910 if in northern California

st_write(TRI_2020_form_wtd_geom, "data/TRI_2020.shp",append=FALSE)
      # using R proj pckg means you can use relative path names not the full path

NEI_2017_form_geom <- st_as_sf(NEI_2017_form, coords = c("longitude", "latitude"), crs = 4269) %>% st_transform(crs=26911) %>%
  st_as_sf()
    # takes long/lat and creates a point geometry tat will work w arcgis pro

st_write(NEI_2017_form_geom, "data/NEI_2017.shp",append=FALSE)

####### now time for arcGIS pro
