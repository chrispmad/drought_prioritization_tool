library(shiny)
library(sf)
library(leaflet)
library(dplyr)

server <- function(input, output, session) {
  
  if(!stringr::str_detect(getwd(),'www$')){
    setwd(paste0(getwd(),'/www/'))
  }
  
  shiny::withProgress(
    message = 'Setting up map',
    detail = 'reading in ecosections...',
    value = 0, {
    
  # Read in ecosections with drought
  ecosecs = sf::read_sf('ecosections_with_drought_sensitivity_and_sar.gpkg')
  
  # ron_id_streams = sf::read_sf('ron_identified_streams.gpkg') |>
  #   sf::st_transform(crs = 4326)

  ron_id_streams = sf::read_sf('ron_streams_layer_v2.shp') |>
    sf::st_transform(crs = 4326)
  # Add in column for colour palette
  
  ron_id_streams = ron_id_streams |> 
    dplyr::mutate(color_val = paste0(Summer_Sen,Winter_Sen))
  
  incProgress(1/3,
              detail = 'Read in ecosections!')
  
  cat('\nread in ecosections')
  
  # Read in all streams from our prioritization model run. (N = 94828)
  # Note that this only includes streams of order 3 or greater.
  # all_streams = sf::read_sf('all_streams_w_drought_risk_and_rons_streams_matched_2.gpkg')
  all_streams = sf::read_sf('streams_simplified.gpkg')
  
  incProgress(1/3,
              detail = 'Read in streams!')
  cat('\nread in streams')
  
  # Read in SAR layer
  sar = sf::read_sf('sar_very_simplified.gpkg') |>
    sf::st_transform(4326)
  
  cat('\nread in SAR layer')
  incProgress(1/3,
              detail = 'Read in Aquatic SAR!')
  
  })

  streams_with_drought_risk = reactiveVal(all_streams)
  
  top_streams_to_show = reactive({
    
    req(!is.null(input$number_streams_to_show))
    
    streams_with_drought_risk() |> 
      dplyr::arrange(dplyr::desc(drought_risk)) |> 
      dplyr::slice(1:input$number_streams_to_show) #|> 
      # rmapshaper::ms_simplify()
  }) |> 
    shiny::debounce(1000)
  
  ecosecs_season_selected = reactive({
    if(input$season_input == 'Summer'){
      ecosecs |> 
        dplyr::rename(sensitivity = summer_sens) |> 
        dplyr::mutate(leaf_colour = case_when(
          sensitivity == 'Not Sensitive' ~ 'lightgreen',
          sensitivity == 'Sensitive Proceed with caution' ~ 'orange',
          sensitivity == 'Very Sensitive--Chronic Problems' ~ '#b8091e',
          T ~ 'grey'
        ))
    } else {
      ecosecs |> 
        dplyr::rename(sensitivity = winter_sens) |> 
        dplyr::mutate(leaf_colour = case_when(
          sensitivity == 'Not Sensitive' ~ 'lightgreen',
          sensitivity == 'Sensitive Proceed with caution' ~ 'orange',
          sensitivity == 'Very Sensitive--Chronic Problems' ~ '#b8091e',
          T ~ 'grey'
        ))
    }
  })
    
  unique_ecosec_names = unique(ecosecs$ECOSECTION_NAME)
  unique_ecosec_names = unique_ecosec_names[order(unique_ecosec_names)]
  
  
  # Reactive for the columns to include in drought calculation
  cols_to_include = reactive({
    
    cols_to_include = c('summer_sens','winter_sens',
                        # 'expert_id',
                        'stream_summer_sens','stream_winter_sens',
                        'number_distinct_SAR',
                        'fish_observed')
    
    if(!input$include_summer_sens){
      cols_to_include = cols_to_include[cols_to_include != 'summer_sens']
    }
    if(!input$include_winter_sens){
      cols_to_include = cols_to_include[cols_to_include != 'winter_sens']
    }
    # if(!input$include_exp_id){
    #   cols_to_include = cols_to_include[cols_to_include != 'expert_id']
    # }
    if(!input$include_stream_summer_sens){
      cols_to_include = cols_to_include[cols_to_include != 'stream_summer_sens']
    }
    if(!input$include_stream_winter_sens){
      cols_to_include = cols_to_include[cols_to_include != 'stream_winter_sens']
    }
    if(!input$include_sar){
      cols_to_include = cols_to_include[cols_to_include != 'number_distinct_SAR']
    }
    if(!input$include_fish_observed){
      cols_to_include = cols_to_include[cols_to_include != 'fish_observed']
    }
    
    cols_to_include
  })
  
  # Listen for a click on 'Rerun Model' button,
  # then rerun our model equation (using weights from
  # the user)
  observeEvent(input$run_model_button, {
    
    # Read in the geopackage containing ALL streams, if it's not been read in
    # already.
    if(!exists('all_streams')){
      all_streams = sf::read_sf('all_streams_w_drought_risk_and_rons_streams_matched_2.gpkg')
    }
    
    sensitivity_factor_levels = c('Not Sensitive',
                                  'Sensitive Proceed with caution',
                                  'Very Sensitive--Chronic Problems')
    
    # This is our code for rerunning the model.
    # i. Calculate drought risk for all streams (is this slow?)
    shiny::withProgress(
      message = 'Recalculating...',
      value = 0, {
        
        incProgress(
          amount = 1/3,
          detail = 'Converting variables to numeric...'
        )

        
    # # Apply weights to variables.
    # all_streams_w_weights = all_streams |> 
    #   dplyr::mutate(summer_sens = input$summer_sens_w_input * as.numeric(factor(summer_sens, levels = sensitivity_factor_levels))-1,
    #                 winter_sens = input$winter_sens_w_input * as.numeric(factor(winter_sens, levels = sensitivity_factor_levels))-1,
    #                 # expert_id = input$exp_id_w_input * expert_id,
    #                 fish_observed = input$fish_obs_w_input * as.numeric(factor(fish_observed, levels = c("N","Y"))) - 1,
    #                 stream_summer_sens = input$stream_summer_sens_w_input * ifelse(stream_summer_sens == 'Y', 1, 0),
    #                 stream_winter_sens = input$stream_winter_sens_w_input * ifelse(stream_winter_sens == 'Y', 1, 0),
    #                 # habitat_value = input$habitat_quality_w_input * as.numeric(factor(habitat_value, levels = c("Low habitat value",
    #                                                                                     # "Medium habitat value",
    #                                                                                     # "High habitat value"))) - 1,
        #                 number_distinct_SAR = input$sar_w_input * number_distinct_SAR)
        # Apply weights to variables.
        all_streams_w_weights = all_streams |> 
          dplyr::mutate(summer_sens = as.numeric(factor(summer_sens, levels = sensitivity_factor_levels))-1,
                        winter_sens = as.numeric(factor(winter_sens, levels = sensitivity_factor_levels))-1,
                        # expert_id = input$exp_id_w_input * expert_id,
                        fish_observed = input$fish_obs_w_input * as.numeric(factor(fish_observed, levels = c("N","Y"))) - 1,
                        stream_summer_sens = ifelse(stream_summer_sens == 'Y', 1, 0),
                        stream_winter_sens = ifelse(stream_winter_sens == 'Y', 1, 0),
                        # habitat_value = input$habitat_quality_w_input * as.numeric(factor(habitat_value, levels = c("Low habitat value",
                        # "Medium habitat value",
                        # "High habitat value"))) - 1,
                        number_distinct_SAR = input$sar_w_input * number_distinct_SAR)
    
    # Use the reactive object that lists the columns the user has flagged to include
    # to include / exclude columns from our all_streams object.
    all_streams_cols_selected = all_streams_w_weights |> 
      dplyr::select(BLUE_LINE_KEY,GNIS_NAME,cols_to_include())
        
    incProgress(
      amount = 1/3,
      detail = 'finding square root of -1...'
    )
    
    stream_drought_risk = all_streams_cols_selected |> 
      sf::st_drop_geometry() |> 
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

    
    incProgress(
      amount = 1/3,
      detail = 'multiplying by -infinity...'
    )
    
    # 2. Just show the X highest-priority streams
    results = all_streams |> 
      # Drop 'old' drought risk column.
      dplyr::select(-drought_risk) |> 
      # Left-join 'new' drought risk column to all_streams
      dplyr::left_join(stream_drought_risk) |> 
      # Arrange by descending drought risk.
      dplyr::arrange(dplyr::desc(drought_risk)) |> 
      # Just take the top N streams.
      dplyr::slice(1:input$number_streams_to_show)
    
    streams_with_drought_risk(results)
      })
  })
  
  # Popup table for streams
  stream_info_tables = reactive({
    leafpop::popupTable(
    top_streams_to_show() |>
      sf::st_drop_geometry() |>
      dplyr::select(
        'Name' = GNIS_NAME,
        'Summer Sensitivity' = summer_sens,
        'Winter Sensitivity' = winter_sens,
        'Stream Summer Sensitivity' = stream_summer_sens,
        'Stream Winter Sensitivity' = stream_winter_sens,
        # 'Expert ID' = expert_id,
        # 'Habitat Value' = habitat_value,
        'Fish Observed' = fish_observed,
        'Number Distinct \nAquatic SAR' = number_distinct_SAR,
        'Drought Risk' = drought_risk
      )
  )
  })
  
  stream_popup_tables = leafpop::popupTable(
    ron_id_streams |> 
      sf::st_drop_geometry() |> 
      dplyr::select(
        StreamName,Summer_Sen,Winter_Sen
        )
  )
  
  # # Make label popup for ecosections.
  ecosection_label = leafpop::popupTable(
      ecosecs |>
        st_drop_geometry() |>
        dplyr::select(Name = ECOSECTION_NAME,
                      `Winter Sensitivity` = winter_sens,
                      `Summer Sensitivity` = summer_sens)
    )
  
  # We made a custom colour palette
  lukecols = c("#804400", "#D7191C", "#ff8f21", "#f7f42f",'lightgreen')
  
  # Make colour palette for ecosections from Ron Ptolemy.
  ecosection_pal = leaflet::colorFactor(palette = 'RdYlGn',
                                        levels = c("Very Sensitive--Chronic Problems",
                                                   "Sensitive Proceed with caution",
                                                   "Not Sensitive"))
  
  
  ron_id_color_pal = leaflet::colorFactor(palette = 'RdYlGn',
                                          levels = c("YY",
                                                     "YN",
                                                     "NY",
                                                     "NN"))
  
  # Make colour palette for high-drought risk streams
  drought_pal = reactive({
    leaflet::colorNumeric(palette = lukecols,
                          reverse = T,
                          domain = top_streams_to_show()$drought_risk)
  })
  
  drought_pal_legend = reactive({
    leaflet::colorNumeric(palette = lukecols,
                          reverse = T,
                          domain = top_streams_to_show()$drought_risk)
  })
  
  # Leaflet map
  output$leaflet_map = renderLeaflet({
    l = leaflet() |> 
      addProviderTiles(provider = providers$CartoDB) |> 
      addMapPane(name = 'ecosec_pane', zIndex = 400) |> 
      addMapPane(name = 'Ron_ID_pane', zIndex = 450) |> 
      addMapPane(name = 'sar_pane', zIndex = 500) |> 
      addMapPane(name = 'stream_pane', zIndex = 600) |> 
      addPolygons(
        data = ecosecs,
        label = ~ECOSECTION_NAME,
        popup = lapply(ecosection_label, htmltools::HTML),
        color = 'darkgrey',
        fillColor = ~ecosection_pal(summer_sens),
        fillOpacity = 0.25,
        group = 'Ecosection - Summer',
        options = pathOptions(pane = 'ecosec_pane')
      ) |> 
      addPolygons(
        data = ecosecs,
        label = ~ECOSECTION_NAME,
        popup = lapply(ecosection_label, htmltools::HTML),
        color = 'darkgrey',
        fillColor = ~ecosection_pal(winter_sens),
        fillOpacity = 0.25,
        group = 'Ecosection - Winter',
        options = pathOptions(pane = 'ecosec_pane')
      ) |> 
      addPolygons(
        label = ~paste0(english_name,'; ',last_obs),
        options = pathOptions(pane = 'sar_pane'),
        group = 'Aquatic SAR',
        col = 'purple',
        fill = 'purple',
        data = sar,
      ) |>
      addPolylines(
        data = ron_id_streams,
        label = ~lapply(stream_popup_tables, htmltools::HTML),
        color = 'lightblue',
        opacity = 1,
        group = 'Expert ID',
        options = pathOptions(pane = 'Ron_ID_pane')
      ) |>
      addLegend(
        title = 'Ecosection Drought Sensitivity',
        group = 'ecosec_legend',
        labels = c('Not Sensitive',
                   'Sensitive Proceed with caution',
                   'Very Sensitive--Chronic Problems'),
        colors = c('lightgreen',
                   'orange',
                   '#b8091e')
      ) |> 
      addLayersControl(position = 'bottomright',
                       overlayGroups = c('Ecosection - Summer','Ecosection - Winter',
                                         'Aquatic SAR','Expert ID','Streams'),
                       options = layersControlOptions(collapsed = F)) |> 
      addScaleBar(position = 'bottomright') |> 
      leaflet.extras::addResetMapButton()
    
    l
  })
  
  observe({
    shiny::withProgress(
      message = 'Updating Map...',
      value = 1/3, {
        
        incProgress(
          amount = 1/3,
          message = 'Updating map.',
          detail = 'Adding streams to map'
        )
        
        leafletProxy('leaflet_map') |> 
          clearGroup('Streams') |> 
          removeControl('stream_legend') |> 
          addPolylines(
            color = ~drought_pal()(drought_risk),
            weight = 5,
            opacity = 0.95,
            label = lapply(
              stream_info_tables(),
              HTML
            ),
            data = top_streams_to_show(),
            group = 'Streams',
            options = pathOptions(pane = 'stream_pane')
          ) |> 
          addLegend(
            title = 'Stream Drought Risk',
            layerId = 'stream_legend',
            pal = drought_pal_legend(),
            values = top_streams_to_show()$drought_risk
          )
        
        incProgress(
          amount = 1/3,
          message = 'Updating map.',
          detail = 'Finished!'
        )
      })
    
    shinyjs::hide(id = "loadingImg",anim = T,time = 3, animType = 'fade')
    
  })
  
  output$download_csv <- downloadHandler(
    filename = function() { 
      paste0('Stream_Drought_Prioritization_',Sys.Date(),'.csv')
      },
    content = function(file) {
      write.csv(
        top_streams_to_show() |> sf::st_drop_geometry(),
        file
      )
    }
  )
  
  output$download_gpkg <- downloadHandler(
    filename = function() { 
      paste0('Stream_Drought_Prioritization_',Sys.Date(),'.gpkg')
    },
    content = function(file) {
      sf::write_sf(
        top_streams_to_show(),
        file
      )
    }
  )
}