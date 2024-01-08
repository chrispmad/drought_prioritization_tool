# PSCIS Assessments
pscis = bcdc_query_geodata('7ecfafa6-5e18-48cd-8d9b-eae5b5ea2881') |> collect()

# =======================
#  Reduce variables in layers      
# =======================

pscis = pscis |> 
  dplyr::select(assessment_date = ASSESSMENT_DATE,
                assessment_id = ASSESSMENT_ID,
                stream_name = STREAM_NAME,
                road_name = ROAD_NAME,
                crossing_type = CROSSING_TYPE_DESC,
                crossing_subtype = CROSSING_SUBTYPE_DESC,
                diameter_or_span = DIAMETER_OR_SPAN,
                length_or_width = LENGTH_OR_WIDTH,
                percent_backwatered = PERCENTAGE_BACKWATERED,
                fill_depth = FILL_DEPTH,
                outlet_drop = OUTLET_DROP,
                outlet_pool_depth = OUTLET_POOL_DEPTH,
                downstream_channel_width = DOWNSTREAM_CHANNEL_WIDTH,
                culvert_slope = CULVERT_SLOPE,
                stream_slope = STREAM_SLOPE,
                fish_observed = FISH_OBSERVED_IND,
                valley_fill = VALLEY_FILL_CODE_DESC,
                habitat_value = HABITAT_VALUE_DESC,
                barrier = BARRIER_RESULT_DESCRIPTION,
                final_score = FINAL_SCORE)

ggplot(pscis) + geom_histogram(aes(final_score))

write_sf(pscis,'raw_data/PSCIS_assessments.gpkg')

