library(sf)
library(leaflet)
library(tidyverse)
library(bcdata)

streams = sf::read_sf("C:/Users/CMADSEN/Downloads/ron_streams_layer_v2.gpkg")

ecos = read_sf("C:/Users/CMADSEN/OneDrive - Government of BC/DroughtFilesForCharlotte/ecosections_with_drought_sensitivity_and_sar_uncorrected.gpkg.gpkg")

nrow(
  ecos |> 
  dplyr::filter(str_detect(ECOSECTION_NAME, 'Halfway'))
)
# Apply corrections to 3 ecosections.
# Trout Lake Plain and Petitot Plain should both be winter and summer sensitive. 
# Northwestern Cascade Ranges should be not sensitive. 
ecos[ecos$ECOSECTION_NAME == 'Northwestern Cascade Ranges',]$summer_sens = 'Not Sensitive'
ecos[ecos$ECOSECTION_NAME == 'Northwestern Cascade Ranges',]$winter_sens = 'Not Sensitive'

ecos = ecos |> 
  dplyr::mutate(summer_sens = ifelse(ECOSECTION_NAME %in% c("Trout Lake Plain","Petitot Plain"), 'Sensitive Proceed with caution', summer_sens),
                winter_sens = ifelse(ECOSECTION_NAME %in% c("Trout Lake Plain","Petitot Plain"), 'Sensitive Proceed with caution', winter_sens))

# Replace the 3-level flow sensitivity system with just 2 levels.
ecos_binary = ecos |> 
  dplyr::mutate(summer_sens = ifelse(summer_sens == 'Not Sensitive','flow insensitive','flow sensitive')) |> 
  dplyr::mutate(winter_sens = ifelse(winter_sens == 'Not Sensitive','flow insensitive','flow sensitive')) |> 
  dplyr::mutate(summer_sens = ifelse(ECOSECTION_NAME %in% c("Trout Lake Plain","Petitot Plain"), 'flow sensitive', summer_sens),
                winter_sens = ifelse(ECOSECTION_NAME %in% c("Trout Lake Plain","Petitot Plain"), 'flow sensitive', winter_sens))

sf::write_sf(ecos_binary, "C:/Users/CMADSEN/OneDrive - Government of BC/DroughtFilesForCharlotte/ecosections_with_2_level_drought_sensitivity_and_sar.gpkg")

ecos = ecos |>
  dplyr::mutate(summer_colour = case_when(
    summer_sens == 'Not Sensitive' ~ 'limegreen',
    summer_sens == 'Sensitive Proceed with caution' ~ 'gold',
    summer_sens == 'Very Sensitive--Chronic Problems' ~ 'red',
    T ~ 'purple'
  )) |>
  dplyr::mutate(winter_colour = case_when(
    winter_sens == 'Not Sensitive' ~ 'limegreen',
    winter_sens == 'Sensitive Proceed with caution' ~ 'gold',
    winter_sens == 'Very Sensitive--Chronic Problems' ~ 'red',
    T ~ 'purple'
  ))

summer_pal = leaflet::colorFactor(palette = 'Spectral', domain=ecos$summer_sens)
winter_pal = leaflet::colorFactor(palette = 'Spectral', domain=ecos$winter_sens)


# SUMMER #
leaflet() |>
  addProviderTiles(provider = providers$CartoDB) |>
  addPolygons(data = ecos,
              label = ~ECOSECTION_NAME,
              fillColor = ~summer_colour,
              # fillColor = ~summer_pal(summer_sens),
              color = 'black',
              weight = 1) #|>
  # addPolylines(data = streams)

# WINTER #
leaflet() |>
  addProviderTiles(provider = providers$CartoDB) |>
  addPolygons(data = ecos,
              label = ~ECOSECTION_NAME,
              fillColor = ~winter_colour,
              # fillColor = ~summer_pal(summer_sens),
              color = 'black',
              weight = 1)

sf::write_sf(ecos, "C:/Users/CMADSEN/OneDrive - Government of BC/DroughtFilesForCharlotte/ecosections_with_drought_sensitivity_and_sar.gpkg")

# 
# str_in_ecosections = tibble(ecosec_name = ecos$ECOSECTION_NAME)
# str_in_ecosections$number_streams_order_3_plus_named = 0
# str_in_ecosections$number_streams_order_3_plus_not_named = 0
# 
# for(i in 74:nrow(str_in_ecosections)){
#   print(i)
#   the_ecosection = ecos[i,] |> sf::st_transform(3005)
#   
#   streams = bcdc_query_geodata("freshwater-atlas-stream-network") |> 
#     filter(STREAM_ORDER >= 3) |> 
#     filter(INTERSECTS(the_ecosection)) |> 
#     collect()
#   
#   streams = streams |> 
#     st_filter(the_ecosection)
#   
#   streams_count = streams |> 
#     sf::st_drop_geometry() |> 
#     dplyr::select(FWA_WATERSHED_CODE, GNIS_NAME) |> 
#     dplyr::distinct() |> 
#     dplyr::mutate(has_name = !is.na(GNIS_NAME)) |> 
#     dplyr::count(has_name, sort = T)
#   
#   # 
#   # output_text = capture.output({
#   #   # Run the bcdc_promise to get the server response
#   #   my_q
#   # })
#   
#   # number_hits = as.numeric(str_extract(output_text[2],"[0-9]+(?= feature)"))
#   if(nrow(streams_count) > 1){
#     str_in_ecosections[i,]$number_streams_order_3_plus_named = streams_count[streams_count$has_name == T,]$n
#   }
#   str_in_ecosections[i,]$number_streams_order_3_plus_not_named = streams_count[!streams_count$has_name,]$n
# }
# 
# str_in_ecosections |> 
#   arrange(ecosec_name) |> 
#   write.csv("C:/Users/CMADSEN/Downloads/LocalR/long_term_projects/drought_prioritization_tool/output/Ecosections_w_number_of_streams_order_three_and_up.csv",
#             row.names = F)
# 
# 
# 
# 
# # How many of the 408 streams cross at least 1 ecosection boundary?
# bcross_str = streams |> 
#   st_join(ecos |> 
#             dplyr::mutate(row_id = row_number()) |> 
#             dplyr::select(row_id, summer_sens, winter_sens)
#   )
# 
# boundaries_crossed_by_stream_name = bcross_str |> 
#   sf::st_drop_geometry() |> 
#   dplyr::count(StreamName, sort = T) |> 
#   dplyr::filter(n > 1)
# #235 streams
# 
# # How many of these streams cross through ecosections of different summer 
# # flow sensitivity?
# bcross_str |> 
#   dplyr::filter(StreamName %in% unique(boundaries_crossed_by_stream_name$StreamName)) |> 
#   dplyr::select(StreamName, summer_sens) |> 
#   sf::st_drop_geometry() |> 
#   dplyr::group_by(StreamName) |> 
#   dplyr::reframe(number_summer_levels = length(unique(summer_sens))) |> 
#   dplyr::count(number_summer_levels, sort = T)
# # 136 streams are in ecosections with the same summer sensitivity
# # 89 are in ecosections with 2 summer sensitivity levels
# # 10 streams are in ecosections with all 3 kinds of sensitivity levels
# 
# bcross_str |> 
#   dplyr::filter(StreamName %in% unique(boundaries_crossed_by_stream_name$StreamName)) |> 
#   dplyr::select(StreamName, winter_sens) |> 
#   sf::st_drop_geometry() |> 
#   dplyr::group_by(StreamName) |> 
#   dplyr::reframe(number_summer_levels = length(unique(winter_sens))) |> 
#   dplyr::count(number_summer_levels, sort = T)
# # 155 streams are in ecosections with the same winter sensitivity
# # 76 are in ecosections with 2 winter sensitivity levels
# # 4 streams are in ecosections with all 3 kinds of sensitivity levels

