# Title: Search BC Data Catalogue for Streams identified in Ron Ptolemy's Drought Sensitive Excel File

# Date: 2024-01-23

# Authors: Luke Eilertsen and Chris Madsen

# Description: Ron Ptolemy (WLRS, Aquatic Ecosystems Branch, Conservation Science Section)
# identified ecosections throughout BC that, based on his datasets, seem drought sensitive.
# Ecosections were ranked from 1 to 3, where 3 means very drought sensitive and 1 means not drought sensitive.
# Specifically, these results were collated by Ron (et al.) in an EFN Report / paper.
# 
# Ron and co. also identified particular streams in each ecosection to serve as examples.

# This script searches the BC Data Catalogue's Freshwater Atlas Stream Network
# for the streams identified by Ron et al., specifically searching by stream name.

# Note that not all streams could be found using this automated approach; 
# Luke E. painstakingly searched for and collated these missing streams into
# a second spatial file ('Rons_DroughtSensitive_Streams_Luke_Compilation.shp', 
# located here: \\\\spatialfiles.bcgov\\work\\wlap\\kam\\Workarea\\LEilertsen\\FlowSensitive_Ecosections_Streams\\FlwSensitive_Ecosec_Streams\\
# and also located here: )

# Update 2024-03-11:
# This file is now somewhat cyclical:
# Because we found an issue in the original code for the automated search,
# (i.e. it cut streams off at the boundary of their identified Ecosection,
# even if they actually continue past said boundary, which is quite common), 
# Luke first corrected many of these cuts by hand, but we found that some 
# streams (e.g. Coldwater River, Spius Creek) remained uncorrected...
# This led us to amend the automatic search code 
# and replace the geometries for streams within Luke's V.1 file 
# and this automated search V.2 that:
# 1. Physically touch (to guarantee they are the same stream)
# 2. Are longer in the automated search V.2 (to guarantee
# it's worth replacing their geometries)

library(tidyverse)
library(bcdata)
library(readxl)
library(sf)

# Read in Ron's list of streams (collated by Luke Eilertsen)
rons_sens_streams = readxl::read_excel('raw_data/FlowSensitiveStreams_RonsEcosections.xlsx')

# Read in the streams that Luke E. traced by hand.

# lukes = sf::read_sf('app/www/ron_identified_streams.gpkg')
lukes = sf::read_sf('raw_data/Rons_FlowSensEcosec_Streams_Merge_FinalBest2.shp') |> 
  sf::st_zm()

if(nrow(lukes) > 1000){
  # This version of Luke's hand-traced streams needs combining by BLUE_LINE_KEY,
  # StreamName, EcoProvince, Ecosection, Summer Sensitivity and Winter Sensitivity.
  lukes = lukes |> 
    dplyr::group_by(BLUE_LINE_,GNIS_NAME,StreamName,EcoProvinc,Ecosection,Summer_Sen,Winter_Sen) |> 
    dplyr::summarise() |> 
    dplyr::ungroup()
}

# Grab the ecosections spatial file
ecosecs = bcmaps::ecosections()

if(!file.exists('output/results_of_bcdata_catalogue_search_for_rons_streams.gpkg')){
ron_streams = map2(
  rons_sens_streams$Stream,
  # 'Coldwater River',
  rons_sens_streams$Ecosection,
  # 'Hozameen Range',
  \(x, y) {
    
    # Attempt to find stream with this name; if none, return nothing.
    tryCatch(
      # Start query of streams from FWA
      expr = {
        streams = bcdc_query_geodata('freshwater-atlas-stream-network') |> 
        # Get streams with this name
        filter(GNIS_NAME == x) |> 
        # Download stream(s)
        collect() |> 
        # Drop Z dimension from stream
        sf::st_zm()
      
        print(paste0('initial cut of streams has ',nrow(streams),' results.'))
        
        # Find the BLUE_LINE_KEY of our target river/stream by looking
        # at the results of the query just inside that ecosection.
        # This should remove streams of the same name that are 
        # unrelated to the identified stream
        stream_blk = streams |> 
          sf::st_join(ecosecs |> 
                        dplyr::select(ecosec = ECOSECTION_NAME) #|> 
                        # dplyr::filter(ecosec == y)
                      ) |> 
          filter(!is.na(ecosec)) |> 
          sf::st_drop_geometry() |> 
          dplyr::select(BLUE_LINE_KEY) |> 
          dplyr::distinct() |> 
          dplyr::slice(1) |> 
          dplyr::pull(BLUE_LINE_KEY)
        
        # This old version tended to cut certain streams if they 
        # crossed ecosection boundaries
        # streams = streams |> 
        #   sf::st_join(ecosecs |> 
        #                 dplyr::select(ecosec = ECOSECTION_NAME) |> 
        #                 dplyr::filter(ecosec == y)) |> 
        #   filter(!is.na(ecosec))
        
        # streams = streams |> 
        #   sf::st_join(ecosecs |> dplyr::select(ecosec = ECOSECTION_NAME)) |> 
        #   filter(!is.na(ecosec))
        
        print(paste0('After st_join with ecosections: N = ',nrow(streams)))
        
        streams = streams |>
          dplyr::filter(BLUE_LINE_KEY == stream_blk) |> 
          dplyr::group_by(BLUE_LINE_KEY, GNIS_NAME) |> 
          dplyr::summarise() |> 
          dplyr::ungroup()
      
      # streams = streams |> dplyr::filter(ecosec == y)
      
      print(paste0('For ',x,', N = ',nrow(streams)))
      
      streams
      },
      error = function(e) NULL
  )
})
    
ron_streams_combined = ron_streams |> 
  bind_rows()

# Do an overlay with ecosections to get a list of ALL ecosections
# that a given stream touches.
stream_ecosections_df = ron_streams_combined |> 
  sf::st_join(ecosecs |> 
                dplyr::select(Ecosections = ECOSECTION_NAME) |> 
                sf::st_transform(3005)) |> 
  sf::st_drop_geometry() |> 
  dplyr::group_by(BLUE_LINE_KEY, GNIS_NAME) |> 
  dplyr::summarise(Ecosections = paste0(Ecosections, collapse = ', '))

ron_streams_combined = ron_streams_combined |> 
  dplyr::left_join(stream_ecosections_df)

# ron_streams_distinct = ron_streams_combined |> 
#   distinct()

write_sf(ron_streams_combined, 'output/results_of_bcdata_catalogue_search_for_rons_streams.gpkg')
} else {
  ron_streams_combined = sf::read_sf('output/results_of_bcdata_catalogue_search_for_rons_streams.gpkg')
}


# For streams that were successfully downloaded using this automatic
# search, replace the geometries for those streams in the current, 'live'
# dataset that the app is using. Save the OG stream dataset for just-in-case
# reasons.

# Which streams physically touch between Luke's original file and 
# the automated search V.2?
ron_luke_stream_join = st_join(
  ron_streams_combined,
  lukes |> 
    dplyr::mutate(id = 'luke_stream') |> 
    dplyr::group_by(BLUE_LINE_,GNIS_NAME,StreamName) |> 
    dplyr::mutate(length_m = sf::st_length(geometry)) |> 
    dplyr::select(StreamName,id,length_m) |> 
    dplyr::distinct() |> 
    dplyr::ungroup()
) |> 
  dplyr::filter(GNIS_NAME == StreamName) |> 
  dplyr::distinct()

# See which streams have longer geometries (at least 150% as long) 
# as a result of the updated automated search code.
updates_for_lukes_stream_geometries = ron_luke_stream_join |> 
  dplyr::mutate(length_from_auto_search = sf::st_length(geom)) |> 
  dplyr::filter(length_from_auto_search > 1.5*length_m) |> 
  dplyr::select(BLUE_LINE_ = BLUE_LINE_KEY,
                StreamName)
  

# Replace the geometries in Luke's V1 file for just the streams 
# identified above.
lukes$uniqID = paste0(lukes$BLUE_LINE_,lukes$StreamName)
updates_for_lukes_stream_geometries$uniqID = paste0(updates_for_lukes_stream_geometries$BLUE_LINE_,
                                                    updates_for_lukes_stream_geometries$StreamName)

lukes_backup = lukes


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## REVISIT THIS FUNCTION to make sure that stream geometry really can be 
# replaced with BC DATA CATALOGUE geometry (e.g. if Ron has split a stream
# in 2 different areas and given them different sensitivities, then 
# replacing both stream sections with the whole stream geometry is no good!)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

for(i in 1:nrow(updates_for_lukes_stream_geometries)){
  
  the_id = updates_for_lukes_stream_geometries[i,]$uniqID
  the_geometry = updates_for_lukes_stream_geometries[i,]$geom
  
  plot(the_geometry, col = 'red')
  plot(lukes[lukes$uniqID == the_id,]$geometry, add = T)
  
  # If there is ONLY one row that matches the BLK and name, 
  # we are probably safe to update the stream geometry with that of the BC Data Catalogue.
  if(nrow(lukes[lukes$uniqID == the_id,]) == 1){
    for(y in 1:nrow(lukes[lukes$uniqID == the_id,])){
      lukes[lukes$uniqID == the_id,][y,]
      lukes[lukes$uniqID == the_id,][y,] = sf::st_set_geometry(lukes[lukes$uniqID == the_id,][y,], the_geometry)
      print('replaced a geometry')
    }
  }
  
  # If there are 2+ rows, and they have different sensitivities, do NOT replace the geometry?
  if(nrow(lukes[lukes$uniqID == the_id,]) > 1){
    
    matched_chunk = lukes[lukes$uniqID == the_id,]
    
    if(length(unique(matched_chunk$Winter_Sen)) == 1 & length(unique(matched_chunk$Summer_Sen)) == 1){
      for(y in 1:nrow(matched_chunk)){
        lukes[lukes$uniqID == the_id,][y,] = sf::st_set_geometry(lukes[lukes$uniqID == the_id,][y,], the_geometry)
        print(paste0('Replaced geometry for a matched chunk with more than 1 row! Name: ',unique(matched_chunk$StreamName)))
      }
    }
  }
  
  # Last thing to check: does the geometry of a "Luke" stream overlap with the
  # geometry of a BC Data Catalogue automatically downloaded stream, but the 
  # BLUE_LINE_KEY values are different? One example I found was 'Cherry Creek', 
  # with 2 rows both in the 'Southern Interior' Ecoprovince; these are clearly different
  # streams but they were assigned the same BLK.
  # if(updates_for_lukes_stream_geometries[i,]$StreamName %in% lukes$StreamName)
}

# Some streams now have full geometries (e.g., Fraser River) but retain
# multiple rows because they originally listed multiple Ecosections.
# If the BLK and listed sensitivities are the same for all rows,
# combine the Ecosections into a list with paste0(collapse = ', ') 
# so that we do not have repeated geometries.
lukes_dups = lukes |> 
  dplyr::filter(duplicated(geometry) & duplicated(Summer_Sen) & duplicated(Winter_Sen) & duplicated(BLUE_LINE_)) |> 
  sf::st_drop_geometry() |> 
  dplyr::pull(BLUE_LINE_)

ecosecs_for_dups = lukes |> 
  dplyr::filter(BLUE_LINE_ %in% lukes_dups) |> 
  sf::st_drop_geometry() |> 
  dplyr::group_by(BLUE_LINE_) |> 
  dplyr::summarise(Ecosections = paste0(Ecosection, collapse = ', ')) |> 
  dplyr::select(BLUE_LINE_,Ecosections)
  
lukes_m = lukes |> 
  dplyr::left_join(ecosecs_for_dups) |> 
  dplyr::mutate(Ecosection = ifelse(!is.na(Ecosections), Ecosections, Ecosection)) |> 
  dplyr::select(-Ecosections) |> 
  distinct()
      
leaflet() |> 
  addTiles() |> 
  addPolylines(
    data = lukes_m |> 
      dplyr::filter(StreamName == 'Cherry Creek') |> 
      sf::st_transform(4326),
    label = ~BLUE_LINE_
  )

new_cherry_creek_geometry = lukes_m |> 
  dplyr::filter(BLUE_LINE_ == 356361174) |> 
  dplyr::summarise()

lukes_m[lukes_m$BLUE_LINE_ == 356361174,]$Ecosection = paste0("Guichon Upland, Thompson Basin")
lukes_m[lukes_m$BLUE_LINE_ == 356361174,] = sf::st_set_geometry(lukes_m[lukes_m$BLUE_LINE_ == 356361174,], rep(new_cherry_creek_geometry$geometry,2))
lukes_m = lukes_m |> 
  dplyr::distinct()

# The code below was used in version 1 of this script to produce
# a table of streams for which the automated search was unsuccessful.
# We do not need to run this part of the script every time.
# unsuccessful_stream_searches = rons_sens_streams |> 
#   dplyr::filter(!paste0(Stream,Ecosection) %in% paste0(ron_streams_combined$GNIS_NAME,
#                                                        ron_streams_combined$ecosec))
# write_csv(unsuccessful_stream_searches, 'output/rons_streams_unsuccessful_digitization.csv')    

# V1 of the automated search:
# sf::write_sf(ron_streams_combined, 'output/ron_streams_layer.gpkg')
# V2, i.e., Luke's stream file with ~86 geometries added to by 
# an improved automated search.

# Drop the uniqID column
lukes_m = lukes_m |> dplyr::select(-uniqID)

sf::write_sf(lukes_m, 'output/ron_streams_layer_v2.gpkg')
sf::write_sf(lukes_m, 'app/www/ron_streams_layer_v2.gpkg')
# sf::write_sf(lukes, 'app/www/ron_streams_layer_V2.shp')
