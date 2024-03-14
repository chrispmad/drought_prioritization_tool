library(sf)
library(tidyverse)

luke = read_sf('raw_data/DroughtSensitiveStreams.shp')

luke = st_zm(luke)

luke = luke |> 
  dplyr::mutate(BLUE_LINE_ = as.character(BLUE_LINE_))

# Merge rows.
luke_s = luke |> 
  dplyr::group_by(BLUE_LINE_,
                  GNIS_NAME,
                  StreamName,
                  EcoProvinc,
                  Ecosection,
                  Summer_Sen,
                  Winter_Sen) |> 
  dplyr::summarise()

luke_ss = rmapshaper::ms_simplify(luke_s)

sf::write_sf(luke_ss, 'app/www/ron_identified_streams.gpkg')

ggplot() + 
  geom_sf(data = luke)
