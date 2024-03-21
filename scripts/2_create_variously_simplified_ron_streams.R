# Load in Ron Streams

# Load libraries
library(sf)
library(tidyverse)
library(rmapshaper)
library(leaflet)

ron_str = read_sf('app/www/ron_streams_layer_v2.gpkg')

# # Quite simplified
# ron_str_simple = ms_simplify(ron_str, keep = 0.01)
# ron_str_simple = st_simplify(ron_str, dTolerance = 0.1)
# 
# ggplot() + geom_sf(data = ron_str_simple)

# Very simplified
# ron_str_v_simple = ms_simplify(ron_str, keep = 0.0001)
ron_str_v_simple = st_simplify(ron_str, dTolerance = 50)

ggplot() + geom_sf(data = ron_str_v_simple)

leaflet() |> 
  addTiles() |> 
  # addPolylines(
  #   data = ron_str_simple
  # ) |> 
  addPolylines(
    data = ron_str_v_simple,
    color = 'purple'
  )

file.remove('app/www/ron_streams_layer_very_simple.gpkg')

sf::write_sf(ron_str_v_simple,
             'app/www/ron_streams_layer_very_simple.gpkg')

object.size(ron_str)/100000
object.size(ron_str_simple)/100000
object.size(ron_str_v_simple)/100000



