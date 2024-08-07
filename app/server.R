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
  
  ron_id_streams = ron_id_streams |> 
    dplyr::arrange(StreamName)
  
  wsc_stations = sf::read_sf('wsc_stations_w_ron_data.gpkg')
  
  # Update an input that depends on stream names.
  stream_names = unique(ron_id_streams$StreamName)
  
  shinyWidgets::updatePickerInput(session = session, 
                                  inputId = "stream_name_search",
                                  choices = stream_names,
                                  selected = NA)
  
  # ron_stream_simplification_amount = reactive({
  #   req(!is.null(input$str_geo_simpl_amount))
  #   input$str_geo_simpl_amount
  # }) |> 
  #   shiny::debounce(millis = 2000)
  
  arrow_icon <- awesomeIcons(
    icon = 'arrow-up',
    iconColor = 'black',
    markerColor = 'red'
  )
  
  # ron_id_streams_simple = reactive({
  #   
  #   req(!is.null(ron_stream_simplification_amount()))
  #   
  #   print("Simplifying Ron's stream file!")
  #   
  #   # Simplify ron's streams' geometries by specified amount
  #   if(ron_stream_simplification_amount() > 0){
  #     # Have one preset simplified version, at, say, 50m downsampling.
  #     if(ron_stream_simplification_amount() == 50){
  #       print("Just reading in Ron's layer simplified to 50m")
  #       # ron_streams_simple = sf::read_sf('ron_streams_layer_very_simple.gpkg') |> 
  #       #   sf::st_transform(crs = 4326) |> 
  #       #   dplyr::mutate(sum_label = ifelse(Summer_Sen == 'Y', 'Yes (<20% MAD)','No (>=20% MAD)'),
  #       #                 wint_label = ifelse(Winter_Sen == 'Y', 'Yes (<20% MAD)','No (>=20% MAD)')) |> 
  #       #   dplyr::mutate(sens_combo = paste0(Summer_Sen,Winter_Sen))
  #       ron_streams_simple = ron_id_streams
  #     } else {
  #       ron_streams_simple = sf::st_simplify(
  #         ron_id_streams,
  #         dTolerance = ron_stream_simplification_amount()
  #       )
  #     }
  #   } else {
  #     ron_streams_simple = ron_id_streams
  #   }
  #   
  #   print("Simplifying finished!")
  #   
  #   ron_streams_simple
  # })
  
  # Do 2 things: listen for the user to pick a radiobutton for Season
  # and adapt data to that choice, and do an overlay of ron's streams
  # with the natural resource regions and, if a region has been selected
  # by dropdown, just show those streams.
  ron_id_st = reactive({
    req(!is.null(input$season_sel))
      if(input$season_sel == 'Summer and Winter'){
        output = ron_id_streams |> 
          dplyr::mutate(leaf_col = case_when(
          sens_combo == 'NN' ~ input$stream_NN_colour,
          sens_combo %in% c('YN','NY') ~ input$stream_some_colour,
          sens_combo == 'YY' ~ input$stream_YY_colour
        ))
      }
      if(input$season_sel == 'Summer'){
        output = ron_id_streams |> 
          dplyr::mutate(leaf_col = case_when(
            sens_combo %in% c('NN','NY') ~ input$stream_NN_colour,
            sens_combo %in% c('YN','YY') ~ input$stream_YY_colour
          ))
      }
      if(input$season_sel == 'Winter'){
        output = ron_id_streams |> 
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
  
  # Reactive form of the WSC stations, pending natural resource region selection.
  wsc_stations_r = reactive({
    output = wsc_stations |> 
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

    coords = ron_id_streams |> 
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
  stream_info_tables = reactive({
    leafpop::popupTable(
      ron_id_st() |>
        sf::st_drop_geometry() |>
        dplyr::select(
          'Name' = GNIS_NAME,
          'Stream Summer Sensitivity' = sum_label,
          'Stream Winter Sensitivity' = wint_label
        )
    )
  })
  
  # Popup table for water survey canada stations
  wsc_info_tables = leafpop::popupTable(
    wsc_stations |> 
      sf::st_drop_geometry()
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
  
  # Make colour palette for water survey canada stations - not sure which var, for now, mad_l_s?
  wsc_pal = leaflet::colorNumeric(palette = 'Spectral',
                                  domain = wsc_stations$mad_l_s,
                                  reverse = TRUE)
  # And the mirror image one for the legend...
  wsc_pal_r = leaflet::colorNumeric(palette = 'Spectral',
                                    domain = wsc_stations$mad_l_s,
                                    reverse = TRUE)
  
  # Leaflet map
  output$leaflet_map = renderLeaflet({
    
    l = leaflet() |> 
      addProviderTiles(provider = providers$CartoDB) |> 
      addMapPane(name = 'nr_regs', zIndex = 300) |> 
      addMapPane(name = 'ecosec_pane', zIndex = 350) |> 
      addMapPane(name = 'Ron_ID_pane', zIndex = 450) |> 
      addMapPane(name = 'highlight_pane', zIndex = 600) |> 
      addMapPane(name = 'wsc_stations', zIndex = 400) |> 
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
      addCircleMarkers(
        data = wsc_stations_r(), 
        label = ~STATION_NAME, 
        popup = lapply(wsc_info_tables, HTML), 
        color = ~wsc_pal(mad_l_s),
        group = 'WSC Stations',
        options = pathOptions(pane = 'wsc_stations')) |> 
      clearGroup('Streams') |> 
      leaflet::removeControl('stream_legend') |> 
      addPolylines(
        data = ron_id_st(),
        label = ~StreamName,
        popup = ~lapply(stream_info_tables(), htmltools::HTML),
        color = ~leaf_col,
        opacity = 1,
        group = 'Streams',
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
        group = 'Ecosection - Winter',
        labels = c('Not Sensitive',
                   'Sensitive Proceed with caution',
                   'Very Sensitive--Chronic Problems'),
        colors = c('lightgreen',
                   'orange',
                   '#b8091e')
      ) |> 
      addLegend(position = 'bottomleft',
                title = 'Water Survey Canada\nMAD (L/s)',
                group = 'WSC Stations',
                pal = wsc_pal_r,
                values = wsc_stations$mad_l_s) |> 
      addLayersControl(position = 'bottomright',
                       overlayGroups = c('Ecosection - Summer',
                                         'Ecosection - Winter',
                                         'WSC Stations',
                                         'NR Regions',
                                         'Streams'),
                       options = layersControlOptions(collapsed = F)) |> 
      addScaleBar(position = 'bottomright') |> 
      leaflet::addEasyButton(button = easyButton(
        icon = 'ion-arrow-shrink',
        title = 'Center map',
        onClick = JS("function(btn, map){ 
                     map.setZoom(5);
                     map.setView([54.55,-126.5],5)
                     }")
      )) |> 
      leaflet::hideGroup('WSC Stations')
    
    # shinyjs::hide(id = "loadingImg",anim = T,time = 3, animType = 'fade')
    # Add legend conditionally
    # Add stream legend
    if(input$season_sel %in% c('Winter','Summer')){
      l = l |> 
        addLegend(
          title = 'Stream Drought Sensitivity',
          group = 'Streams',
          labels = c("Not Sensitive (>= 20% MAD)",
                     "Sensitive (< 20% MAD)"),
          layerId = 'stream_legend',
          colors = c(
            input$stream_NN_colour,
            input$stream_YY_colour
          )
        )
    }
    if(input$season_sel == 'Summer and Winter'){
      l = l |> 
        addLegend(
          title = 'Stream Drought Sensitivity',
          group = 'Streams',
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
    if(!is.null(input$stream_name_search) | input$ptolemy_page == 'Tool'){
      l = leafletProxy('leaflet_map') |>
        clearGroup('highlight_box') #|> 
        # clearGroup('Streams') |> 
        # leaflet::removeControl('stream_legend') |> 
        # addPolylines(
        #   data = ron_id_st(),
        #   label = ~StreamName,
        #   popup = ~lapply(stream_info_tables, htmltools::HTML),
        #   color = ~leaf_col,
        #   opacity = 1,
        #   group = 'Streams',
        #   options = pathOptions(pane = 'Ron_ID_pane')
        # )
      
      if(!is.null(searched_stream_name())){
        if(searched_stream_name() != 'NA'){
          
          l = l |>
            addMarkers(data = highlight_data(),
                       group = 'highlight_box',
                       options = pathOptions(pane = 'highlight_pane')
            )
        }
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
  
  name_for_stream_downloads = reactive({
    if(input$nr_reg_search == 'Whole Province'){
      name = paste0('Ptolemy_Stream_Drought_Prioritization_',Sys.Date())
    } else {
      name = paste0('Ptolemy_Stream_Drought_Prioritization_',
                    input$nr_reg_search,'_',
                    Sys.Date())
    }
    name
  })
  
  name_for_wsc_downloads = reactive({
    if(input$nr_reg_search == 'Whole Province'){
      name = paste0('Ptolemy_WSC_Drought_Prioritization_',Sys.Date())
    } else {
      name = paste0('Ptolemy_WSC_Drought_Prioritization_',
                    input$nr_reg_search,'_',
                    Sys.Date())
    }
    name
  })
  
  # Stream CSV Download
  output$download_csv <- downloadHandler(
    filename = function() { 
      paste0(name_for_stream_downloads(),'.csv')
      },
    content = function(file) {
      write.csv(
        ron_id_st() |> sf::st_drop_geometry(),
        file
      )
    }
  )
  
  # Stream Geopackage Download
  output$download_gpkg <- downloadHandler(
    filename = function() { 
      paste0(name_for_stream_downloads(),'.gpkg')
    },
    content = function(file) {
      sf::write_sf(
        ron_id_st(),
        file
      )
    }
  )
  
  # Ecosection CSV download
  output$download_ecos_csv <- downloadHandler(
    filename = function() {
      paste0('Ptolemy_Ecosection_Drought_Prioritization_',Sys.Date(),'.csv')
    },
    content = function(file) {
      write.csv(
        ecosecs |> sf::st_drop_geometry(),
        file
      )
    }
  )
  
  # Ecosection Geopackage Download
  output$download_ecos_gpkg <- downloadHandler(
    filename = function() {
      paste0('Ptolemy_Ecosection_Drought_Prioritization_',Sys.Date(),'.gpkg')
    },
    content = function(file) {
      sf::write_sf(
        ecosecs,
        file
      )
    }
  )
  
  # Ptolemy Data at WSC station Download
  output$download_wsc_csv <- downloadHandler(
    filename = function() { 
      paste0(name_for_wsc_downloads(),'.csv')
    },
    content = function(file) {
      write.csv(
        wsc_stations_r() |> sf::st_drop_geometry(),
        file
      )
    }
  )
  
  output$download_wsc_gpkg <- downloadHandler(
    filename = function() { 
      paste0(name_for_wsc_downloads(),'.gpkg')
    },
    content = function(file) {
      sf::write_sf(
        wsc_stations_r(),
        file
      )
    }
  )
  
  output$download_EFN_paper = downloadHandler(
    filename = function() { 
      paste0("RPtolemy_EFN_Hydrology_Paper.zip")
    },
    content = function(file) {
      files_to_zip <- c("ref_docs/EFN_Hydrology_Paper_New.docx", 
                        "ref_docs/Hydrology_Sup_Figures.docx", 
                        "ref_docs/Figure Captions Hydrology_Main.docx")
      # Full paths to the files to be zipped
      full_paths <- file.path(files_to_zip)
      # Zip the files
      zip(file, files = full_paths)
    }
  )
}