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
  
  # Read in Flow sensitive streams from Ron's ecosection excel file.
  # rons_sens_streams = readxl::read_excel('FlowSensitiveStreams_RonsEcosections.xlsx') |> 
  #   dplyr::rename(GNIS_NAME = Stream,
  #                 summer_sens_expert_id = `Summer Sensitive`,
  #                 winter_sens_expert_id = `Winter Sensitive`) |> 
  #   dplyr::mutate(summer_sens_expert_id = ifelse(summer_sens_expert_id == 'Y', 1, 0),
  #                 winter_sens_expert_id = ifelse(winter_sens_expert_id == 'Y', 1, 0))
  
  incProgress(1/3,
              detail = 'Read in ecosections!')
  cat('\nread in ecosections')
  
  # Read in all streams from our prioritization model run. (N = 94828)
  # Note that this only includes streams of order 3 or greater.
  all_streams = sf::read_sf('all_streams_w_drought_risk.gpkg')
  
  # all_streams = all_streams |> 
  #   dplyr::left_join(rons_sens_streams) |> 
  #   dplyr::mutate(summer_sens_expert_id = tidyr::replace_na(summer_sens_expert_id, 0),
  #                 winter_sens_expert_id = tidyr::replace_na(winter_sens_expert_id, 0))
  
  incProgress(1/3,
              detail = 'Read in streams!')
  cat('\nread in streams')
  
  # Read in SAR layer
  # sar = sf::read_sf('sar_very_simplified.gpkg') |> 
  #   sf::st_transform(4326)
  
  cat('\nread in SAR layer')
  incProgress(1/3,
              detail = 'Read in Aquatic SAR!')
  
  })
  
  # top_200 = sf::read_sf('top_200_streams_by_drought_risk.gpkg')
  
  streams_with_drought_risk = reactiveVal(all_streams)
  
  top_streams_to_show = reactive({
    
    req(!is.null(input$number_streams_to_show))
    
    streams_with_drought_risk() |> 
      dplyr::arrange(dplyr::desc(drought_risk)) |> 
      dplyr::slice(1:input$number_streams_to_show)
  })
  
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
    
    # col_filter_table = data.frame(
    #   variable_name = c('summer_sens','winter_sens','number_distinct_SAR','fish_observed','habitat_value'),
    #   input_name = c('include_summer_sens','include_winter_sens','include_sar','include_fish_observed','include_habitat_quality')
    # )
    cols_to_include = c('summer_sens','winter_sens',
                        #'summer_sens_expert_id','winter_sens_expert_id',
                        'summer_sens_expert_id_num','winter_sens_expert_id_num',
                        'number_distinct_SAR',
                        'fish_observed','habitat_value')
    
    if(!input$include_summer_sens){
      cols_to_include = cols_to_include[cols_to_include != 'summer_sens']
    }
    if(!input$include_winter_sens){
      cols_to_include = cols_to_include[cols_to_include != 'winter_sens']
    }
    if(!input$include_exp_id_winter_sens){
      cols_to_include = cols_to_include[cols_to_include != 'winter_sens_expert_id_num']
    }
    if(!input$include_exp_id_summer_sens){
      cols_to_include = cols_to_include[cols_to_include != 'summer_sens_expert_id_num']
    }
    if(!input$include_sar){
      cols_to_include = cols_to_include[cols_to_include != 'number_distinct_SAR']
    }
    if(!input$include_fish_observed){
      cols_to_include = cols_to_include[cols_to_include != 'fish_observed']
    }
    if(!input$include_habitat_quality){
      cols_to_include = cols_to_include[cols_to_include != 'habitat_value']
    }
    
    
    # col_filter_table |> 
    #   dplyr::filter(input[[input_name]]) |> 
    #   dplyr::pull(variable_name)
    cols_to_include
  })
  
  # Listen for a click on 'Rerun Model' button,
  # then rerun our model equation (using weights from
  # the user)
  observeEvent(input$run_model_button, {
    
    # Read in the geopackage containing ALL streams, if it's not been read in
    # already.
    if(!exists('all_streams')){
      all_streams = sf::read_sf('all_streams_w_drought_risk.gpkg')
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
        
    if(is.character(all_streams$summer_sens_expert_id)){
      all_streams = all_streams |> 
        dplyr::mutate(summer_sens_expert_id_num = ifelse(summer_sens_expert_id == 'Y', 1, 0),
                      winter_sens_expert_id_num = ifelse(winter_sens_expert_id == 'Y', 1, 0))
    }
        
    # Apply weights to variables.
    all_streams_w_weights = all_streams |> 
      dplyr::mutate(summer_sens = input$summer_sens_w_input * as.numeric(factor(summer_sens, levels = sensitivity_factor_levels))-1,
                    winter_sens = input$winter_sens_w_input * as.numeric(factor(winter_sens, levels = sensitivity_factor_levels))-1,
                    summer_sens_expert_id_num = input$exp_id_summer_sens_w_input * summer_sens_expert_id_num,
                    winter_sens_expert_id_num = input$exp_id_winter_sens_w_input * winter_sens_expert_id_num,
                    fish_observed = input$fish_obs_w_input * as.numeric(factor(fish_observed, levels = c("N","Y"))) - 1,
                    habitat_value = input$habitat_quality_w_input * as.numeric(factor(habitat_value, levels = c("Low habitat value",
                                                                                        "Medium habitat value",
                                                                                        "High habitat value"))) - 1,
                    number_distinct_SAR = input$sar_w_input * number_distinct_SAR)
    
    # Use the reacive object that lists the columns the user has flagged to include
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
    
    # 2. Add drought risk table to the spatial object.
    # all_streams_cols_selected = all_streams_cols_selected |> 
    #   dplyr::left_join(stream_drought_risk)
    
    # 3. Just show the X highest-priority streams
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
  # sf::read_sf('../drought_prioritization_tool/app/www/streams/fraser_lowland_simple.gpkg') |> View()
  
  # # Use the ecosection with drought sensitivity gpkg to inform ecosec_input
  # shinyWidgets::updatePickerInput(
  #   session = session,
  #   'ecosec_input',
  #   choices = c('province',unique(unique_ecosec_names)),
  #   selected = 'province'
  # )
  # 
  # ecosec_choice = reactiveVal('province')
  # 
  # observeEvent(input$ecosec_input, {
  #   ecosec_choice(input$ecosec_input)
  # })
  # 
  # observeEvent(input$leaflet_map_shape_click, {
  #   req(!is.null(input$leaflet_map_shape_click))
  #   shinyWidgets::updatePickerInput(
  #     session,
  #     'ecosec_input',
  #     selected = input$leaflet_map_shape_click$id
  #   )
  # })
  
  # # Stream data - reactive choice
  # stream_dat = reactive({
  #   req(ecosec_choice() != 'province')
  #   
  #   stream_filename = paste0(stringr::str_replace_all(stringr::str_to_lower(ecosec_choice()),' ','_'),'_simple.gpkg')
  #   
  #   dat = sf::read_sf(paste0('streams/',stream_filename))
  #   
  #   if(input$season_input == 'Summer'){
  #     dat |> 
  #       dplyr::rename(sensitivity = summer_sens)
  #   } else {
  #     dat |> 
  #       dplyr::rename(sensitivity = winter_sens)
  #   }
  # })
  
  # # Palette for streams.
  # stream_pal = reactive({
  #   leaflet::colorNumeric(
  #     palette = 'RdYlGn',
  #     domain = stream_dat() |> 
  #       sf::st_drop_geometry() |> 
  #       dplyr::select(input$var_to_display_input),
  #     reverse = T
  #   )
  # })
  
  # # Palette for streams - legend version. Ugh.
  # stream_legend_pal = reactive({
  #   leaflet::colorNumeric(
  #     palette = 'RdYlGn',
  #     domain = stream_dat() |> 
  #       sf::st_drop_geometry() |> 
  #       dplyr::select(input$var_to_display_input) |> 
  #       dplyr::pull(input$var_to_display_input),
  #     reverse = F
  #   )
  # })
  
  # # Popup table for streams
  # stream_info = reactive({
  #   leafpop::popupTable(
  #     stream_dat() |> 
  #       dplyr::select(Name = GNIS_NAME,
  #                     `Drought Sensitivity` = sensitivity,
  #                     `Assessment Date` = assessment_date,
  #                     `Fish Observed` = fish_observed,
  #                     `Crossing Type` = crossing_type,
  #                     `Crossing Subtype` = crossing_subtype,
  #                     Barrier = barrier,
  #                     `Habitat Value` = habitat_value,
  #                     `Priority Rating` = priority_rating) |> 
  #       sf::st_drop_geometry()
  #   )
  # })
  
  # Make label popup for ecosections.
  ecosection_label = leafpop::popupTable(
      ecosecs |> 
        st_drop_geometry() |> 
        dplyr::select(Name = ECOSECTION_NAME, 
                      `Winter Sensitivity` = winter_sens, 
                      `Summer Sensitivity` = summer_sens)
    )
  
  # We made a custom colour palette
  lukecols = c("#804400", "#D7191C", "#ff8f21", "#f7f42f")
  
  # Make colour palette for ecosections from Ron Ptolemy.
  ecosection_pal = leaflet::colorFactor(palette = 'RdYlGn',
                                        levels = c("Very Sensitive--Chronic Problems",
                                                   "Sensitive Proceed with caution",
                                                   "Not Sensitive"))
  
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
      # addTiles() |> 
      addProviderTiles(provider = providers$CartoDB) |> 
      addPolygons(
        data = ecosecs,
        label = ~ECOSECTION_NAME,
        popup = lapply(ecosection_label, htmltools::HTML),
        color = 'darkgrey',
        fillColor = ~ecosection_pal(summer_sens),
        fillOpacity = 0.25,
        group = 'summer_bm'
      ) |> 
      addPolygons(
        data = ecosecs,
        label = ~ECOSECTION_NAME,
        popup = lapply(ecosection_label, htmltools::HTML),
        color = 'darkgrey',
        fillColor = ~ecosection_pal(winter_sens),
        fillOpacity = 0.25,
        group = 'winter_bm'
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
                       overlayGroups = c('ecosecs','streams','sar'),
                       baseGroups = c('winter_bm','summer_bm'),
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
        
        stream_info_tables = leafpop::popupTable(
          top_streams_to_show() |> 
            sf::st_drop_geometry() |> 
            dplyr::select(
              'Name' = GNIS_NAME,
              'Summer Sensitivity' = summer_sens,
              'Winter Sensitivity' = winter_sens,
              'Expert ID Summer Sens.' = summer_sens_expert_id,
              'Expert ID Winter Sens.' = winter_sens_expert_id,
              'Habitat Value' = habitat_value,
              'Fish Observed' = fish_observed,
              'Number Distinct \nAquatic SAR' = number_distinct_SAR,
              'Drought Risk' = drought_risk
            )
        )
        
        leafletProxy('leaflet_map') |> 
          clearGroup('streams') |> 
          removeControl('stream_legend') |> 
          addPolylines(
            color = ~drought_pal()(drought_risk),
            weight = 5,
            opacity = 0.95,
            # label = ~paste0(GNIS_NAME,', ',drought_risk),
            label = lapply(
              stream_info_tables,
              HTML
            ),
            data = top_streams_to_show(),
            group = 'streams'
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
  })
  
  # Download Buttons.
  # output$download_plot <- downloadHandler(
  #     filename = function() {
  #       paste0('plot_', Sys.Date(), '.png')
  #     },
  #     content = function(file) {
  #       write.csv(data, file)
  #     }
  #   )
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