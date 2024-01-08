library(sf)
library(tidyverse)

# =======================
#    Settings variables      
# =======================
sensitivity_factor_levels = c('Not Sensitive',
                              'Sensitive Proceed with caution',
                              'Very Sensitive--Chronic Problems')

# =======================
#     Reading in Data      
# =======================

# Ecosections
ecosecs = bcmaps::ecosections()

ecosecs = ecosecs |> 
  dplyr::select(ECOSECTION_NAME)

# Ecosections with drought sensitivities
ecosecs_w_d = read_sf('W:/CMadsen/shared_data_sets/ecosec_with_drought_sensitivity.gpkg')

# Streams - this section will read in raw data and 
# process it ONLY if the outputs are not already present. This saves 
# us a lot of time in subseqent runs.

if(!file.exists('raw_data/streams_w_drought.gpkg')){
  streams = sf::read_sf('raw_data/Stream_Drought_Prioritization.gdb')
  
  # Drop Z dimension (height) for streams.
  streams = sf::st_zm(streams)
  
  sf::write_sf(streams, 'raw_data/streams_w_drought.gpkg')
} else {
  if(!file.exists(paste0('streams_by_ecosec/streams_alsek_ranges.gpkg'))){
    
    streams = sf::read_sf('raw_data/streams_w_drought.gpkg')
    
    # Find the highest winter and summer sensitivity for each BLUE_LINE_KEY
    # Apply this highest sensitivity to BLUE_LINE_KEY.
    # all_unique_blk = unique(streams$BLUE_LINE_KEY)
    
    winter_table = streams |> 
      st_drop_geometry() |> 
      distinct(BLUE_LINE_KEY, winter_sens) |> 
      mutate(winter_sens = factor(winter_sens, levels = sensitivity_factor_levels)) |> 
      arrange(BLUE_LINE_KEY, desc(winter_sens)) |> 
      group_by(BLUE_LINE_KEY) |> 
      slice(1) |> 
      dplyr::rename(highest_winter = winter_sens)
    
    summer_table = streams |> 
      st_drop_geometry() |> 
      distinct(BLUE_LINE_KEY, summer_sens) |> 
      mutate(summer_sens = factor(summer_sens, levels = sensitivity_factor_levels)) |> 
      arrange(BLUE_LINE_KEY, desc(summer_sens)) |> 
      group_by(BLUE_LINE_KEY) |> 
      slice(1) |> 
      dplyr::rename(highest_summer = summer_sens)
    
    streams = streams |> 
      left_join(winter_table)
    
    streams = streams |> 
      left_join(summer_table)
    
    streams = streams |> 
      mutate(winter_sens = highest_winter) |> 
      dplyr::select(-highest_winter)
      
    streams = streams |> 
      mutate(summer_sens = highest_summer) |> 
      dplyr::select(-highest_summer)
    
    rm(summer_table)
    rm(winter_table)
    
    for(ecosec_name in ecosecs$ECOSECTION_NAME){
      
      this_ecosec_snake = snakecase::to_snake_case(ecosec_name)
      
      streams_in_ecosec = streams |> 
        dplyr::filter(ECOSECTION_NAME == ecosec_name)
      
      write_sf(streams_in_ecosec, paste0('streams_by_ecosec/streams_',this_ecosec_snake,'.gpkg'))
    }
  }
  rm(streams)
  rm(streams_in_ecosec)
  gc()
}

# Carry out a filtering step on the streams:
# Drop streams of stream order 1

# I tried keeping streams of order 1 if the ecosection 
# was at all sensitive to drought, but it kept too many streams!

for(ecosec_name in ecosecs$ECOSECTION_NAME){
  
  this_ecosec_snake = snakecase::to_snake_case(ecosec_name)
  
  streams_in_ecosec = sf::read_sf(paste0('streams_by_ecosec/streams_',this_ecosec_snake,'.gpkg'))
  
  # Remove lake-defining skeletons and 
  streams_in_ecosec = streams_in_ecosec |> 
    filter(STREAM_ORDER > 2) |> 
    filter(!FEATURE_SOURCE %in% c("lake-def skelet","OP"))
  
  sf::write_sf(streams_in_ecosec, paste0('streams_by_ecosec/streams_',this_ecosec_snake,'.gpkg'))
  
  # if(interactive()){
  # ggplot() + 
  #   geom_sf(data = ecosecs_w_d |> 
  #             filter(ECOSECTION_NAME == ecosec_name),
  #           aes(fill = summer_sens)) +
  #   geom_sf(data = streams_in_ecosec |> 
  #             filter(STREAM_ORDER >= 6) |> 
  #             sf::st_transform(4326),
  #           aes(col = summer_sens)) + 
  #   scale_color_manual(values = c("Not Sensitive" = 'gold',
  #                                 "Very Sensitive--Chronic Problems" = 'red')) +
  #   scale_fill_manual(values = c("Not Sensitive" = 'lightblue',
  #                                "Very Sensitive--Chronic Problems" = 'red'))
  # }
  
  
}

