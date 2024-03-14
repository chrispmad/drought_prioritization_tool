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
  
  incProgress(1/2, detail = 'Read in ecosections')

  ron_id_streams = sf::read_sf('ron_streams_layer_v2.shp') |>
    sf::st_transform(crs = 4326) |> 
    dplyr::mutate(sum_label = ifelse(Summer_Sen == 'Y', 'Yes (<20% MAD)','No (>=20% MAD)'),
                  wint_label = ifelse(Winter_Sen == 'Y', 'Yes (<20% MAD)','No (>=20% MAD)')) |> 
    dplyr::mutate(sens_combo = paste0(Summer_Sen,Winter_Sen)) |> 
    dplyr::mutate(leaf_col = case_when(
      sens_combo == 'NN' ~ 'darkblue',
      sens_combo %in% c('YN','NY') ~ 'yellow',
      sens_combo == 'YY' ~ 'red'
    ))
  
  incProgress(1/2, detail = 'Read in Rons streams')
  
  })

  selected_streams = reactive({
    ron_id_streams
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
  
  # # Stream colour palette
  # stream_pal = leaflet::colorFactor(palette = 'RlYlGn',
  #                      domain = unique(ron_id_streams$sens_combo))
  
  # Make colour palette for ecosections from Ron Ptolemy.
  ecosection_pal = leaflet::colorFactor(palette = 'RdYlGn',
                                        levels = c("Very Sensitive--Chronic Problems",
                                                   "Sensitive Proceed with caution",
                                                   "Not Sensitive"))
  
  
  # Leaflet map
  output$leaflet_map = renderLeaflet({
    l = leaflet() |> 
      addProviderTiles(provider = providers$CartoDB) |> 
      addMapPane(name = 'ecosec_pane', zIndex = 400) |> 
      addMapPane(name = 'Ron_ID_pane', zIndex = 450) |> 
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
      addPolylines(
        data = ron_id_streams,
        label = ~StreamName,
        popup = ~lapply(stream_info_tables, htmltools::HTML),
        color = ~leaf_col,
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
      addLegend(
        title = 'Stream Drought Sensitivity',
        labels = c("Not Summer or Winter",
                   "Summer or Winter",
                   "Summer and Winter"),
        colors = c('darkblue','yellow','red')
      ) |> 
      addLayersControl(position = 'bottomright',
                       overlayGroups = c('Ecosection - Summer','Ecosection - Winter',
                                         'Expert ID'),
                       options = layersControlOptions(collapsed = F)) |> 
      addScaleBar(position = 'bottomright') |> 
      leaflet.extras::addResetMapButton()
    
    shinyjs::hide(id = "loadingImg",anim = T,time = 3, animType = 'fade')
    
    l
  })
  
  output$download_csv <- downloadHandler(
    filename = function() { 
      paste0('Stream_Drought_Prioritization_',Sys.Date(),'.csv')
      },
    content = function(file) {
      write.csv(
        selected_streams() |> sf::st_drop_geometry(),
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
        selected_streams(),
        file
      )
    }
  )
}