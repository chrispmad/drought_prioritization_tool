# Process stream geopackage (one per ecosection)
# This includes:
# 1. Filling in name gaps (GNIS_NAME field)
# 2. Merging streams based on their BLUE_LINE_KEY (unique identifier)

clean_and_merge_streams = function(dat){
  # Are there are streams split into multiple pieces, where one piece has 
  # a name and the other piece(s) don't?
  stream_name_filler_table = dat |> 
    st_drop_geometry() |> 
    count(BLUE_LINE_KEY, GNIS_NAME, sort = T) |> 
    add_count(BLUE_LINE_KEY) |> 
    filter(nn > 1) |> 
    mutate(GNIS_NAME = ifelse(GNIS_NAME == "Unknown", NA, GNIS_NAME)) |> 
    group_by(BLUE_LINE_KEY) |> 
    mutate(corrector = paste0(GNIS_NAME, collapse = ', ')) |> 
    mutate(corrector = str_remove_all(corrector, ', NA')) |> 
    mutate(GNIS_NAME_corrector = corrector) |> 
    ungroup() |> 
    dplyr::select(BLUE_LINE_KEY, GNIS_NAME_corrector) |> 
    distinct()
  
  dat = dat |> 
    left_join(stream_name_filler_table) |> 
    mutate(GNIS_NAME = ifelse(!is.na(GNIS_NAME_corrector), GNIS_NAME_corrector, GNIS_NAME))
  
  # Merge streams by BLUE_LINE_KEY (and GNIS_NAME, if available)
  # Where we had multiple pieces for a single stream, now they will be combined.
  dat = dat |> 
    dplyr::group_by(BLUE_LINE_KEY, GNIS_NAME, summer_sens, winter_sens) |> 
    dplyr::summarise()
  
  dat
}


## Function to do a spatial overlay and then clean up duplications of streams
## that are caused by multiple matches of the second layer with the first layer.
spatial_overlay_join = function(dat,
                                overlay_layer,
                                final_score_variable_name){
  dat = dat |> 
    sf::st_join(overlay_layer, sf::st_intersects) |> 
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) tidyr::replace_na(x, 0))) |> 
    dplyr::mutate(dplyr::across(dplyr::where(is.character), \(x) tidyr::replace_na(x, 'Unknown')))
  
  dupes = dat |> 
    dplyr::add_count(BLUE_LINE_KEY) |> 
    dplyr::filter(n > 1)
  
  dupes = dupes |> 
    dplyr::group_by(BLUE_LINE_KEY) |> 
    dplyr::arrange(dplyr::desc(!!sym(final_score_variable_name))) |> 
    dplyr::slice(1) |> 
    dplyr::ungroup()
  
  dat = dat |> 
    dplyr::filter(!BLUE_LINE_KEY %in% dupes$BLUE_LINE_KEY) |> 
    dplyr::bind_rows(dupes)
}

stream_prioritization_model = function(dat,
                                       variables = list(),
                                       levels = list()){
  dat = dat |> 
    sf::st_drop_geometry()
  
  priority_table = map2(variables, levels, ~ {
    if(!'numeric' %in% unlist(.y)){
      dat |> 
        dplyr::select(!!sym(.x)) |> 
        dplyr::mutate(!!sym(.x) := as.numeric(factor(!!sym(.x), levels = .y)))
    } else {
      dat |> 
        dplyr::select(!!sym(.x))
    }
  }) |> dplyr::bind_cols()
  
  priority_table$BLUE_LINE_KEY = dat$BLUE_LINE_KEY
  priority_table$GNIS_NAME = dat$GNIS_NAME
  
  browser()
  stream_priorities = priority_table |> 
    pivot_longer(cols = c(unlist(variables))) |> 
    dplyr::select(BLUE_LINE_KEY,GNIS_NAME,name,value) |> 
    group_by(BLUE_LINE_KEY) |> 
    mutate(value = as.numeric(value)) |>
    dplyr::mutate(value = dplyr::case_when(
      # Reduce the numeric value of summer and winter sensitivities so that the scale
      # starts at 0, i.e. so that 'Not Sensitive' is 0, not 1.
      name %in% c('summer_sens','winter_sens') ~ value - 1, 
      T ~ value)
      ) |> 
    mutate(priority_rating = sum(value, na.rm=T)) |> 
    dplyr::select(BLUE_LINE_KEY, GNIS_NAME, priority_rating) |> 
    dplyr::distinct()
  
  stream_priorities
}
