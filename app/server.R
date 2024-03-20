library(shiny)
library(sf)
library(leaflet)
library(dplyr)

server <- function(input, output, session) {
  
  if(!stringr::str_detect(getwd(),'www$')){
    setwd(paste0(getwd(),'/www/'))
  }
  
  # Read in ecosections with drought
  ecosecs = sf::read_sf('ecosections_with_drought_sensitivity_and_sar.gpkg')
  
  # Read in Natural Resource Regions.
  nr_regs = sf::read_sf('nr_regions.gpkg') |> 
    sf::st_transform(4326)
  
  ron_id_streams = sf::read_sf('ron_streams_layer_very_simple.gpkg') |>
    sf::st_transform(crs = 4326) |> 
    dplyr::mutate(sum_label = ifelse(Summer_Sen == 'Y', 'Yes (<20% MAD)','No (>=20% MAD)'),
                  wint_label = ifelse(Winter_Sen == 'Y', 'Yes (<20% MAD)','No (>=20% MAD)')) |> 
    dplyr::mutate(sens_combo = paste0(Summer_Sen,Winter_Sen))
  
  # Update an input that depends on stream names.
  stream_names = unique(ron_id_streams$StreamName)[order(unique(ron_id_streams$StreamName))]
  
  shinyWidgets::updatePickerInput(session = session, 
                                  inputId = "stream_name_search",
                                  choices = stream_names,
                                  selected = NA)
  
  ron_stream_simplification_amount = reactive({
    req(!is.null(input$str_geo_simpl_amount))
    input$str_geo_simpl_amount
  }) |> 
    shiny::debounce(millis = 2000)
  
  arrow_icon <- awesomeIcons(
    icon = 'arrow-up',
    iconColor = 'black',
    markerColor = 'red'
  )
  
  ron_id_streams_simple = reactive({
    
    req(!is.null(ron_stream_simplification_amount()))
    
    print("Simplifying Ron's stream file!")
    
    # Simplify ron's streams' geometries by specified amount
    if(ron_stream_simplification_amount() > 0){
      # Have one preset simplified version, at, say, 50m downsampling.
      if(ron_stream_simplification_amount() == 50){
        print("Just reading in Ron's layer simplified to 50m")
        # ron_streams_simple = sf::read_sf('ron_streams_layer_very_simple.gpkg') |> 
        #   sf::st_transform(crs = 4326) |> 
        #   dplyr::mutate(sum_label = ifelse(Summer_Sen == 'Y', 'Yes (<20% MAD)','No (>=20% MAD)'),
        #                 wint_label = ifelse(Winter_Sen == 'Y', 'Yes (<20% MAD)','No (>=20% MAD)')) |> 
        #   dplyr::mutate(sens_combo = paste0(Summer_Sen,Winter_Sen))
        ron_streams_simple = ron_id_streams
      } else {
        ron_streams_simple = sf::st_simplify(
          ron_id_streams,
          dTolerance = ron_stream_simplification_amount()
        )
      }
    } else {
      ron_streams_simple = ron_id_streams
    }
    
    print("Simplifying finished!")
    
    ron_streams_simple
  })
  
  # Do 2 things: listen for the user to pick a radiobutton for Season
  # and adapt data to that choice, and do an overlay of ron's streams
  # with the natural resource regions and, if a region has been selected
  # by dropdown, just show those streams.
  ron_id_st = reactive({
    req(!is.null(ron_id_streams_simple()))
      if(input$season_sel == 'Both'){
        output = ron_id_streams_simple() |> 
          dplyr::mutate(leaf_col = case_when(
          sens_combo == 'NN' ~ input$stream_NN_colour,
          sens_combo %in% c('YN','NY') ~ input$stream_some_colour,
          sens_combo == 'YY' ~ input$stream_YY_colour
        ))
      }
      if(input$season_sel == 'Summer'){
        output = ron_id_streams_simple() |> 
          dplyr::mutate(leaf_col = case_when(
            sens_combo %in% c('NN','NY') ~ input$stream_NN_colour,
            sens_combo %in% c('YN','YY') ~ input$stream_YY_colour
          ))
      }
      if(input$season_sel == 'Winter'){
        output = ron_id_streams_simple() |> 
          dplyr::mutate(leaf_col = case_when(
            sens_combo %in% c('NN','YN') ~ input$stream_NN_colour,
            sens_combo %in% c('NY','YY') ~ input$stream_YY_colour
          ))
      }
    
    # Spatial match with natural resource regions
    output = output |> 
      sf::st_join(nr_regs)
    
    if(input$nr_reg_search != 'Whole Province'){
      output = output |> 
        dplyr::filter(stringr::str_remove(REGION_NAME,' Natural.*') == input$nr_reg_search)
    }
    output
  })
  
  # A reactive form of the natural resource regions, to highlight
  # selection.
  nr_regs_h = reactive({
    if(input$nr_reg_search == 'Whole Province'){
      output = nr_regs |> 
        dplyr::mutate(leaf_colour = 'black') |> 
        dplyr::mutate(leaf_opac = 1)
    } else {
      output = nr_regs |> 
      dplyr::mutate(
        leaf_colour = case_when(
          stringr::str_remove(REGION_NAME, ' Natural.*') == input$nr_reg_search ~ 'black',
          T ~ 'lightgrey'),
        leaf_opac = case_when(
          stringr::str_remove(REGION_NAME, ' Natural.*') == input$nr_reg_search ~ 1,
          T ~ 0.5)
      )
    }
    output
  })
  
  # A specifically searched stream - make a bounding box for it?
  searched_stream_name = reactiveVal('NA')
  
  observe({
    searched_stream_name(input$stream_name_search)
  })
  
  observeEvent(input$reset_name_search, {
    searched_stream_name('NA')
    shinyWidgets::updatePickerInput(session = session, 
                                    inputId = "stream_name_search",
                                    selected = 'NA')
  })
  
  highlight_data = reactive({
    req(searched_stream_name() != 'NA')

    coords = ron_id_streams_simple() |> 
      dplyr::filter(StreamName == searched_stream_name()) |> 
      sf::st_boundary() |> 
      sf::st_cast("POINT") |> 
      dplyr::slice(1) |> 
      sf:: st_coordinates() |> 
      as.data.frame()
    
    names(coords) <- c('lng','lat')
    
    coords
  })
  
  # Popup table for streams
  stream_info_tables = leafpop::popupTable(
    ron_id_streams |>
      sf::st_drop_geometry() |>
      dplyr::select(
        'Name' = GNIS_NAME,
        'Stream Summer Sensitivity' = sum_label,
        'Stream Winter Sensitivity' = wint_label
      )
  )
  
  # Make label popup for ecosections.
  ecosection_label = leafpop::popupTable(
      ecosecs |>
        st_drop_geometry() |>
        dplyr::select(Name = ECOSECTION_NAME,
                      `Winter Sensitivity` = winter_sens,
                      `Summer Sensitivity` = summer_sens)
    )
  
  # Make colour palette for ecosections from Ron Ptolemy.
  ecosection_pal = leaflet::colorFactor(palette = 'RdYlGn',
                                        levels = c("Very Sensitive--Chronic Problems",
                                                   "Sensitive Proceed with caution",
                                                   "Not Sensitive"))
  
  # Leaflet map
  output$leaflet_map = renderLeaflet({
    
    l = leaflet() |> 
      addProviderTiles(provider = providers$CartoDB) |> 
      addMapPane(name = 'nr_regs', zIndex = 300) |> 
      addMapPane(name = 'ecosec_pane', zIndex = 400) |> 
      addMapPane(name = 'Ron_ID_pane', zIndex = 450) |> 
      addMapPane(name = 'highlight_pane', zIndex = 600) |> 
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
      clearGroup('Expert ID') |> 
      leaflet::removeControl('stream_legend') |> 
      addPolylines(
        data = ron_id_st(),
        label = ~StreamName,
        popup = ~lapply(stream_info_tables, htmltools::HTML),
        color = ~leaf_col,
        opacity = 1,
        group = 'Expert ID',
        options = pathOptions(pane = 'Ron_ID_pane')
      ) |> 
      addPolygons(
        data = nr_regs_h(),
        label = ~REGION_NAME,
        color = ~leaf_colour,
        opacity = ~leaf_opac,
        fillColor = 'transparent',
        weight = 2,
        group = 'NR Regions',
        options = pathOptions(pane = 'nr_regs')
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
                       overlayGroups = c('Ecosection - Summer',
                                         'Ecosection - Winter',
                                         'NR Regions',
                                         'Expert ID'),
                       options = layersControlOptions(collapsed = F)) |> 
      addScaleBar(position = 'bottomright') |> 
      leaflet.extras::addResetMapButton()
    
    # shinyjs::hide(id = "loadingImg",anim = T,time = 3, animType = 'fade')
    # Add legend conditionally
    # Add stream legend
    if(input$season_sel %in% c('Winter','Summer')){
      l = l |> 
        addLegend(
          title = 'Stream Drought Sensitivity',
          labels = c("Not Sensitive (>= 20% MAD)",
                     "Sensitive (< 20% MAD)"),
          layerId = 'stream_legend',
          colors = c(
            input$stream_NN_colour,
            input$stream_YY_colour
          )
        )
    }
    if(input$season_sel == 'Both'){
      l = l |> 
        addLegend(
          title = 'Stream Drought Sensitivity',
          labels = c("Not Sensitive (>= 20% MAD)",
                     "Winter or Summer Sensitive (< 20% MAD)",
                     "Winter <b>and</b> Summer Sensitive (< 20% MAD)"),
          layerId = 'stream_legend',
          colors = c(
            input$stream_NN_colour,
            input$stream_some_colour,
            input$stream_YY_colour
          )
        )
    }
    
    # If we already have a leaflet map center and zoom, use those to inform 
    # new map rendering.
    isolate(
      if(!is.null(leaf_zoom())){
      l = l |> 
        leaflet::setView(lng = leaf_lng(),
                         lat = leaf_lat(),
                         zoom = leaf_zoom())
      }
    )
    
    l |> 
      leaflet::hideGroup(c('Ecosection - Winter',
                         'Ecosection - Summer'))
  })
  
  observe({
    if(!is.null(input$stream_name_search)){
      l = leafletProxy('leaflet_map') |>
        clearGroup('highlight_box') #|> 
        # clearGroup('Expert ID') |> 
        # leaflet::removeControl('stream_legend') |> 
        # addPolylines(
        #   data = ron_id_st(),
        #   label = ~StreamName,
        #   popup = ~lapply(stream_info_tables, htmltools::HTML),
        #   color = ~leaf_col,
        #   opacity = 1,
        #   group = 'Expert ID',
        #   options = pathOptions(pane = 'Ron_ID_pane')
        # )
      
      if(searched_stream_name() != 'NA'){

        l = l |>
          addMarkers(data = highlight_data(),
                     group = 'highlight_box',
                     options = pathOptions(pane = 'highlight_pane')
                    )
      }
      
      l
    }
  })
  
  # ReactiveVals for leaflet lat, lng and zoom. If these aren't blank,
  # we'll use them to inform a new leaflet render.
  
  leaf_lat = reactiveVal()
  leaf_lng = reactiveVal()
  leaf_zoom = reactiveVal()
  
  observe({
    leaf_lat(input$leaflet_map_center$lat)
    leaf_lng(input$leaflet_map_center$lng)
    leaf_zoom(input$leaflet_map_zoom)
  })
  
  name_for_downloads = reactive({
    if(input$nr_reg_search == 'Whole Province'){
      name = paste0('Stream_Drought_Prioritization_',Sys.Date())
    } else {
      name = paste0('Stream_Drought_Prioritization_',
                    input$nr_reg_search,'_',
                    Sys.Date())
    }
    name
  })
  
  output$download_csv <- downloadHandler(
    filename = function() { 
      paste0(name_for_downloads(),'.csv')
      },
    content = function(file) {
      write.csv(
        ron_id_st() |> sf::st_drop_geometry(),
        file
      )
    }
  )
  
  output$download_gpkg <- downloadHandler(
    filename = function() { 
      paste0(name_for_downloads(),'.gpkg')
    },
    content = function(file) {
      sf::write_sf(
        ron_id_st(),
        file
      )
    }
  )
}