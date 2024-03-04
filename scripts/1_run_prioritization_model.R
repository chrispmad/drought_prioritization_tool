# Title: Run Prioritization Model

# Date: 2024-03-04

# Authors: Luke Eilertsen and Chris Madsen

# Running this script will update the data files of the Stream Drought Sensitivity App.

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


# Load libraries
library(sf)
library(tidyverse)

# Read in data files.

# Species at risk (publically available) (pulling from another project I have)
sar = sf::read_sf('../species_at_risk_visualizer/app/www/species_ecosystems_at_risk_publically_available_no_insects.gpkg') |> 
  st_transform(3005)

# PSCIS Assessments
pscis = sf::read_sf('raw_data/PSCIS_assessments.gpkg')

# Ecosections
ecosecs = read_sf('app/www/ecosections_with_drought_sensitivity.gpkg')

# Ron Ptolemy's hand-selected streams and rivers, digitized by Luke Eilertsen.
rons_streams = sf::read_sf('app/www/ron_identified_streams.gpkg')

rons_streams_blk = rons_streams |> 
  dplyr::filter(BLUE_LINE_ != '0')

rons_streams_no_blk = rons_streams |> 
  dplyr::filter(BLUE_LINE_ == '0')

# Other layers...

# =========================
#  Modifying spatial files   
# =========================

pscis_b = pscis |> 
  sf::st_buffer(dist = 50) # Buffer by 50 meters

sensitivity_factor_levels = c('Not Sensitive',
                              'Sensitive Proceed with caution',
                              'Very Sensitive--Chronic Problems')


# =======================
#   Apply model
# =======================

# name_mismatches = data.frame(
#   stream_name = c(""),
#   ecosection_name = c("")
# )

# ecosec_name = ecosecs$ECOSECTION_NAME[1]

for(ecosec_name in ecosecs$ECOSECTION_NAME){
  
  print(which(ecosecs$ECOSECTION_NAME == ecosec_name))
  
  this_ecosec_snake = snakecase::to_snake_case(ecosec_name)
  
  streams_in_ecosec = sf::read_sf(paste0('intermediate_data/streams_by_ecosec/streams_',this_ecosec_snake,'.gpkg'))
  
  # Merge streams by BLUE_LINE_KEY (and GNIS_NAME, if available)
  streams_in_ecosec = streams_in_ecosec |> 
    dplyr::group_by(BLUE_LINE_KEY, GNIS_NAME, summer_sens, winter_sens) |> 
    dplyr::summarise()
  
  # Spatial overlay of pscis assessment info.
  streams_in_ecosec = streams_in_ecosec |> 
    st_join(pscis, st_intersects) |> 
    mutate(across(where(is.numeric), \(x) replace_na(x, 0))) |> 
    mutate(across(where(is.character), \(x) replace_na(x, 'Unknown')))
  
  # Spatial overlay of SAR.
  initial_spatial_match = streams_in_ecosec |> 
    st_join(sar |> 
              select(SCI_NAME, ENG_NAME, TAX_CLASS), st_intersects)
  
  rows_pasted_together = initial_spatial_match |> 
    dplyr::group_by(BLUE_LINE_KEY, GNIS_NAME, summer_sens, winter_sens) |> 
    dplyr::mutate(SCI_NAME = paste0(SCI_NAME, collapse = ', '),
                  ENG_NAME = paste0(ENG_NAME, collapse = ', ')
    ) |> 
    ungroup()
  
  number_distinct_SAR_by_BLK = initial_spatial_match |> 
    sf::st_drop_geometry() |> 
    group_by(BLUE_LINE_KEY, GNIS_NAME, summer_sens, winter_sens) |> 
    dplyr::select(SCI_NAME) |> 
    dplyr::filter(!is.na(SCI_NAME)) |> 
    dplyr::distinct() |> 
    dplyr::summarise(number_distinct_SAR = n())
  
  streams_in_ecosec = rows_pasted_together |> 
    dplyr::select(-TAX_CLASS) |> 
    group_by(BLUE_LINE_KEY, GNIS_NAME, summer_sens, winter_sens) |> 
    dplyr::slice(1) |> 
    ungroup() |> 
    dplyr::left_join(number_distinct_SAR_by_BLK)
  
  # Simple left join for those of Ron's select streams that have BLUE_LINE_KEY.
  streams_in_ecosec = streams_in_ecosec |> 
    dplyr::left_join(
      rons_streams_blk |> 
        dplyr::mutate(BLUE_LINE_KEY = as.integer(BLUE_LINE_)) |> 
        sf::st_drop_geometry() |> 
        dplyr::filter(!duplicated(BLUE_LINE_KEY)) |> 
        dplyr::select(BLUE_LINE_KEY, 
                      stream_summer_sens = Summer_Sen, 
                      stream_winter_sens = Winter_Sen)
    )
  
  # For those streams that did not have BLUE_LINE_KEY, spatial overlay.
  stream_spatial_match = sf::st_join(
    streams_in_ecosec,
    rons_streams_no_blk |> 
      dplyr::select(stream_summer_sens_spatial = Summer_Sen, 
                    stream_winter_sens_spatial = Winter_Sen)
  ) |> 
    dplyr::filter(!is.na(stream_summer_sens_spatial))
  
  if(nrow(stream_spatial_match) > 0){
    streams_in_ecosec = streams_in_ecosec |> 
      # Left join the result of spatial matching, just with new columns and BLK
      dplyr::left_join(
        stream_spatial_match |> 
          sf::st_drop_geometry() |> 
          dplyr::filter(!is.na(BLUE_LINE_KEY)) |> 
          dplyr::select(BLUE_LINE_KEY,
                        stream_summer_sens_spatial,
                        stream_winter_sens_spatial) |> 
          dplyr::filter(!duplicated(BLUE_LINE_KEY))
      ) |> 
      # If there is data in the spatial matching columns of summer and winter sensitivity,
      # use those to replace the NA from the initial left_join we used above.
      dplyr::mutate(stream_summer_sens = ifelse(is.na(stream_summer_sens),stream_summer_sens_spatial,stream_summer_sens),
                    stream_winter_sens = ifelse(is.na(stream_winter_sens),stream_winter_sens_spatial,stream_winter_sens)
                    )
  }
  
  # This chunk of code adds together choice variables to estimate an overall 
  # drought risk.
  streams_in_ecosec_w_numeric = streams_in_ecosec |> 
    ungroup() |> 
    st_drop_geometry() |> 
    # Convert variables to factors; this let's us add them together.
    mutate(summer_sens_numeric = as.numeric(factor(summer_sens, levels = sensitivity_factor_levels))-1,
           winter_sens_numeric = as.numeric(factor(winter_sens, levels = sensitivity_factor_levels))-1,
           stream_summer_sens_numeric = ifelse(stream_summer_sens == 'Y', 1, 0),
           stream_winter_sens_numeric = ifelse(stream_winter_sens == 'Y', 1, 0),
           fish_observed_numeric = as.numeric(factor(fish_observed, levels = c("N","Y"))) - 1,
           habitat_value_numeric = as.numeric(factor(habitat_value, levels = c("Low habitat value",
                                                            "Medium habitat value",
                                                            "High habitat value"))) - 1)
  
  stream_drought_risk = streams_in_ecosec_w_numeric |> 
    pivot_longer(cols = c(summer_sens_numeric,
                          winter_sens_numeric,
                          stream_summer_sens_numeric,
                          stream_winter_sens_numeric,
                          fish_observed_numeric,
                          habitat_value_numeric,
                          number_distinct_SAR)) |> 
    dplyr::select(BLUE_LINE_KEY,GNIS_NAME,name,value) |> 
    group_by(BLUE_LINE_KEY,GNIS_NAME) |> 
    mutate(drought_risk = sum(value, na.rm=T)) |> 
    dplyr::select(-c(name,value)) |> 
    dplyr::distinct() |> 
    ungroup()
  
  # Join the calculated drought risk to the streams spatial file.
  streams_in_ecosec = streams_in_ecosec |> 
    dplyr::left_join(stream_drought_risk)
  
  # Simplify stream geometries.
  streams_in_ecosec = tryCatch(
    rmapshaper::ms_simplify(streams_in_ecosec),
    error = function(e) return(streams_in_ecosec)
  )
  
  # Reproject the streams to WGS 84.
  streams_in_ecosec = sf::st_transform(streams_in_ecosec, crs = 4326)
  
  sf::write_sf(streams_in_ecosec, paste0('intermediate_data/streams_by_ecosec_with_drought_info/',this_ecosec_snake,'_simple.gpkg'))
}

# Add them all together too!
all_streams = list.files('intermediate_data/streams_by_ecosec_with_drought_info/',full.names = T) |> 
  lapply(\(x)
         sf::read_sf(x)) |> 
  bind_rows()

# Because some rivers/streams span multiple ecosections, we have to grab the 
# highest / most pertinent value for some columns and apply them to all rows 
# of a given GNIS_NAME / BLUE_LINE_KEY.
all_streams_c = all_streams |> 
  group_by(GNIS_NAME, BLUE_LINE_KEY) |> 
  mutate(
    drought_risk = max(replace_na(drought_risk,0), na.rm=T),
    number_distinct_SAR = max(replace_na(number_distinct_SAR,0), na.rm=T)
         ) |> 
  ungroup()

sf::write_sf(all_streams_c, 'output/all_streams_corrected.gpkg')


################
#  RESUME HERE #
#  We're changing from 2 columns for winter and summer expert ID, which 
#  were using Ron's excel table and basic name matching, to now use
#  spatialized streams from Ron's list that we make in another script.
#  So, we just want one column called something like 'expert_id'.
#  There are duplicate matches so have to think about how to clean those up.

# rons_streams = sf::read_sf('output/ron_streams_layer.gpkg')

# all_streams_c = sf::read_sf('output/all_streams_corrected.gpkg')
# 
# test = all_streams_c |> 
#   st_join(
#     rons_streams |> 
#       dplyr::mutate(expert_id = 1) |> 
#       select(expert_id, GNIS_NAME_ron = GNIS_NAME) |> 
#       sf::st_transform(4326)
#   )

all_streams_choice_cols = all_streams_c |> 
  dplyr::select(drought_risk,
                BLUE_LINE_KEY,
                GNIS_NAME,
                number_distinct_SAR,
                SCI_NAME,
                ENG_NAME,
                stream_winter_sens,
                stream_summer_sens,
                summer_sens,
                winter_sens,
                fish_observed
                )

write_sf(all_streams_choice_cols, 'app/www/all_streams_w_drought_risk_and_rons_streams_matched_2.gpkg')

# test$expert_id

##### CODE BELOW HAS NOT BEEN TESTED OR RERUN IN A WHILE
  
nrow(test)
# Make sure that 
all_streams_w_weights = all_streams_c |> 
  dplyr::mutate(summer_sens = as.numeric(factor(summer_sens, levels = sensitivity_factor_levels))-1,
                winter_sens = as.numeric(factor(winter_sens, levels = sensitivity_factor_levels))-1,
                winter_sens_expert_id = ifelse(winter_sens_expert_id == "Y", 1, 0),
                summer_sens_expert_id = ifelse(summer_sens_expert_id == "Y", 1, 0),
                fish_observed = as.numeric(factor(fish_observed, levels = c("N","Y"))) - 1,
                habitat_value = as.numeric(factor(habitat_value, levels = c("Low habitat value",
                                                                            "Medium habitat value",
                                                                            "High habitat value"))) - 1,
                number_distinct_SAR = number_distinct_SAR)

# Calculate drought risk table.
stream_drought_risk = all_streams_w_weights |> 
  sf::st_drop_geometry() |> 
  dplyr::select(BLUE_LINE_KEY,GNIS_NAME,habitat_value,summer_sens,winter_sens,winter_sens_expert_id,summer_sens_expert_id,number_distinct_SAR,fish_observed) |> 
  tidyr::pivot_longer(cols = -c(BLUE_LINE_KEY, GNIS_NAME)) |> 
  dplyr::select(BLUE_LINE_KEY,GNIS_NAME,name,value) |> 
  dplyr::filter(!is.na(value)) |> 
  dplyr::distinct() |> 
  # In the case of multiple values, just keep the highest one for each BLK and GNIS_NAME
  dplyr::group_by(BLUE_LINE_KEY,GNIS_NAME,name) |> 
  dplyr::slice_max(value) |> 
  dplyr::ungroup() |> 
  dplyr::group_by(BLUE_LINE_KEY,GNIS_NAME) |> 
  dplyr::mutate(drought_risk = sum(value, na.rm=T)) |> 
  dplyr::select(-c(name,value)) |> 
  dplyr::distinct() |> 
  dplyr::ungroup()

# 2. Add drought risk table to the spatial object.
all_streams_c = all_streams_c |> 
  dplyr::select(BLUE_LINE_KEY,GNIS_NAME,habitat_value,summer_sens,winter_sens,winter_sens_expert_id,summer_sens_expert_id,number_distinct_SAR,fish_observed) |> 
  dplyr::left_join(stream_drought_risk)

# top_200 = all_streams |> 
#   dplyr::arrange(desc(drought_risk)) |> 
#   dplyr::slice(1:200)
# 
# sf::write_sf(top_200, 'app/www/top_200_streams_by_drought_risk.gpkg')

sf::write_sf(all_streams_c |> 
               dplyr::select(BLUE_LINE_KEY, GNIS_NAME, drought_risk, 
                             summer_sens, winter_sens, 
                             winter_sens_expert_id,summer_sens_expert_id,
                             habitat_value,
                             fish_observed, number_distinct_SAR), 'app/www/all_streams_w_drought_risk.gpkg')
