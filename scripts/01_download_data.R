# Title: Download Data
#
# Date: 2024-03-30
#
# Author(s): Chris Madsen (chris.madsen@gov.bc.ca)
# 
# Description: This script downloads some datasets that the Shiny app depends on.

library(bcdata)
library(tidyverse)
library(bcmaps)
library(sf)
library(tidyhydat)

nr_regs = st_simplify(bcmaps::nr_regions(), dTolerance = 100) |> 
  dplyr::select(REGION_NAME, ORG_UNIT_NAME)

sf::write_sf(nr_regs, 'app/www/nr_regions.gpkg')            

# Read in Ron's 'Hydromaster' excel sheet
wsc_ron_data = openxlsx::read.xlsx('raw_data/Ron_HydroMaster.xlsx',sheet = 'ALL DATA') |> 
  as_tibble()

new_names = paste0(wsc_ron_data[2,])
new_names[c(18,19,21,22)] <- c("Max_Summer","Min_Summer","Max_Winter","Min_Winter")

# Drop empty rows
wsc_ron_data = wsc_ron_data[c(3:nrow(wsc_ron_data)),]
# Apply new column names
names(wsc_ron_data) <- new_names

# Drop two 'NA' columns at end.
wsc_ron_data = wsc_ron_data[,1:c(ncol(wsc_ron_data)-2)]

# Make into snakecase.
wsc_ron_data = wsc_ron_data |> 
  purrr::set_names(snakecase::to_snake_case) |> 
  dplyr::rename(STATION_NAME = stream)

# Drop repeated rows with the same station name
wsc_ron_data = wsc_ron_data |> 
  dplyr::filter(!duplicated(STATION_NAME))

# Get the stations from {tidyhydat}
bc_stations = tidyhydat::realtime_stations(prov_terr_state_loc = 'BC')

bc_stations = bc_stations |> 
  dplyr::filter(STATION_NAME %in% unique(wsc_ron_data$stream))

stats_w_dat = bc_stations |> 
  sf::st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4326) |> 
  dplyr::left_join(wsc_ron_data) |> 
  dplyr::select(-c(PROV_TERR_STATE_LOC,TIMEZONE,wsc,eco_province,eco_section,eco_section_code,eco_region_code,order))

# Make the numbers a bit nicer to look at.
stats_w_dat = stats_w_dat |> 
  dplyr::mutate(across(c(years_record,da_km_2,mad_l_s,runoff_l_s_km_2,mm_yr,`20_mad`,summer_cpsf_mad,max_summer,min_summer,
                         winter_cpsf_mad,max_winter,min_winter,lwy_fraction,hwy_fraction), \(x) round(as.numeric(x),2)))

sf::write_sf(stats_w_dat,
             'app/www/wsc_stations_w_ron_data.gpkg')
