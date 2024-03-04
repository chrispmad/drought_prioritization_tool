# This script is for combining the all_streams geopackage
# and adding on Ron's hand-selected stream sensitivities in summer and winter.

library(tidyverse)

rons_sens_streams = readxl::read_excel('app/www/FlowSensitiveStreams_RonsEcosections.xlsx') |> 
  dplyr::rename(GNIS_NAME = Stream,
                summer_sens_expert_id = `Summer Sensitive`,
                winter_sens_expert_id = `Winter Sensitive`) |> 
  dplyr::mutate(summer_sens_expert_id = ifelse(summer_sens_expert_id == 'Y', 1, 0),
                winter_sens_expert_id = ifelse(winter_sens_expert_id == 'Y', 1, 0))

ecosecs = sf::read_sf('app/www/ecosections_with_drought_sensitivity_and_sar.gpkg')

all_streams = sf::read_sf('app/www/all_streams_w_drought_risk.gpkg')

all_streams_w_ecoprov = all_streams |> 
  st_join(ecosecs |> dplyr::select(EcoSection = ECOSECTION_NAME),
          st_intersects)

