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

library(tidyverse)
library(bcdata)
library(readxl)

# Read in Ron's list of streams (collated by Luke Eilertsen)
rons_sens_streams = readxl::read_excel('raw_data/FlowSensitiveStreams_RonsEcosections.xlsx')

# Grab the ecosections spatial file
ecosecs = bcmaps::ecosections()

ron_streams = map2(
  rons_sens_streams$Stream,
  rons_sens_streams$Ecosection, \(x, y) {
    
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
        
        streams = streams |> 
          sf::st_join(ecosecs |> 
                        dplyr::select(ecosec = ECOSECTION_NAME) |> 
                        dplyr::filter(ecosec == y)) |> 
          filter(!is.na(ecosec))
        
        print(paste0('After st_join with ecosections: N = ',nrow(streams)))
        
        streams = streams |> 
        dplyr::group_by(ecosec, BLUE_LINE_KEY, GNIS_NAME) |> 
        dplyr::summarise() |> 
        dplyr::ungroup()
      
      streams = streams |> dplyr::filter(ecosec == y)
      
      print(paste0('For ',x,', N = ',nrow(streams)))
      
      streams
      },
      error = function(e) NULL
  )
})
    
ron_streams_combined = ron_streams |> 
  bind_rows()

leaflet() |> 
  leaflet::addPolylines(
    data = ron_streams_combined |> 
      sf::st_transform(crs = 4326)
  )
unsuccessful_stream_searches = rons_sens_streams |> 
  dplyr::filter(!paste0(Stream,Ecosection) %in% paste0(ron_streams_combined$GNIS_NAME,
                                                       ron_streams_combined$ecosec))

sf::write_sf(ron_streams_combined, 'output/ron_streams_layer.gpkg')
write_csv(unsuccessful_stream_searches, 'output/rons_streams_unsuccessful_digitization.csv')    
