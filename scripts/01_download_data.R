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

nr_regs = st_simplify(bcmaps::nr_regions(), dTolerance = 100) |> 
  dplyr::select(REGION_NAME, ORG_UNIT_NAME)

sf::write_sf(nr_regs, 'app/www/nr_regions.gpkg')            
