library(shiny)
library(bslib)
library(leaflet)
library(leafpop)
library(capture)

# =======================
#   Define UI pieces      
# =======================

## Join Ron's "Hydromaster" document, it's on OneDrive.
## It looks like this could be joined to water survey Canada 
## stations (like in Flowmaster 3000). Would the leaflet map
## be too busy? Maybe.

season_selector = radioButtons(
  'season_sel',
  'Season Selector',
  choices = c("Summer","Winter","Both"),
  selected = c("Both")
)

stream_by_name_selector = shinyWidgets::pickerInput(
  inputId = "stream_name_search",
  label = "Stream Name", 
  choices = NA,
  options = list(
    `live-search` = TRUE
    )
)

stream_geometry_simplification = sliderInput(
  'str_geo_simpl_amount',
  'Stream Geometry Simplification (m)',
  value = 10,
  min = 0, 
  max = 100, 
  step = 5
)

stream_nn_picker = shinyWidgets::colorPickr(
  'stream_NN_colour',
  label = 'Not Drought Sensitive',
  selected = 'darkblue'
)

stream_some_picker = shinyWidgets::colorPickr(
  'stream_some_colour',
  label = 'Not Drought Sensitive',
  selected = 'yellow'
)

stream_yy_picker = shinyWidgets::colorPickr(
  'stream_YY_colour',
  label = 'Not Drought Sensitive',
  selected = 'red'
)

the_accordion = accordion(
  id = 'sidebar_accordion',
  accordion_panel(
    title = 'Selectors',
    season_selector,
    stream_by_name_selector
  ),
  accordion_panel(
    title = 'Visual Options',
    stream_nn_picker,
    stream_some_picker,
    stream_yy_picker,
    stream_geometry_simplification
  )
)

sidebar = div(
  the_accordion,
  layout_column_wrap(
    1/3,
    capture::capture(
      selector = "body",
      filename = paste0("Stream_Drought_Prioritization_",Sys.Date(),"_screenshot.png"),
      icon("camera"), "Capture",
      style = 'padding:10px;display:grid;height:10vh;',
      class = "btn-info"
    ),
    downloadButton('download_csv',shiny::HTML('Spreadsheet'),
                   style = 'padding:10px;display:grid;height:10vh;',
                   class = "btn-warning"
    ),
    downloadButton('download_gpkg','\n.GPKG',
                   style = 'padding:10px;display:grid;height:10vh;',
                   class = "btn-secondary")
  ),
  style = 'padding-top:10px;'
)

# Main page with map

main = div(
  # tags$div(
  #   id = "loadingImg",
  #   tags$img(id = "loadingImg", 
  #            src = "map_load_still_complete.png", 
  #            style = "position:absolute;width:145vh;
  #          height:90vh;top:10.5vh;left:60vh;z-index:1000;"),
  #   tags$div(
  #     style = 'position:absolute;width:145vh;background-color:grey;opacity:0.5;height:90vh;top:10.5vh;left:60vh;z-index:1100;'
  #   ),
  #   tags$p('Loading... please wait 1 - 2 minutes...',
  #          style = 'position:absolute;top:55vh;left:120vh;z-index:1101;font-family:fantasy;font-size:larger;')
  # ),
    leafletOutput('leaflet_map',
                height = '90vh'
                )
)
    
ui <- page_navbar(
  shinyjs::useShinyjs(),
  theme = bslib::bs_theme(preset = 'flatly'),
  # title = 'Stream Drought Sandbox',
  title = 'Ptolemy Tool: Drought-Sensitive Stream Dataset',
  selected = 'Tool',
  bslib::nav_panel(
    title = 'Tool',
    shinyFeedback::useShinyFeedback(),
    fluidRow(
      column(width = 3,
             sidebar
             ),
      column(width = 9,
             main
             )
    )
  ),
  bslib::nav_panel(
    title = 'Metadata',
    h1("Data Sources"),
    h3("BC Data Catalogue"),
    h5("1. Conservation Data Center"),
    p("Publically available Aquatic Species-at-risk layer"),
    h5('2. Streams from the Freshwater Atlas layer'),
    p("Stream network lines (only steams of order 3 or greater included)"),
    h5('3. PSCIS (Provincial Stream Crossing Inventory System)'),
    p('Describes various measures of fish passage, habitat quality, waterway morphology.'),
    h3("Provincial Hydrologist Ron Ptolemy"),
    h5('1. Drought Sensitivity of Ecosections'),
    HTML("<br><br>"),
    h3("Contact"),
    h5("Luke Eilertsen"),
    p('Luke.Eilertsen@gov.bc.ca'),
    h5("Chris Madsen"),
    p('Chris.Madsen@gov.bc.ca')
  ),
  nav_item(
    bslib::input_dark_mode()
  )
)
