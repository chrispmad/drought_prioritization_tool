library(shiny)
library(bslib)
library(leaflet)
library(leafpop)
library(capture)

# =======================
#   Define UI pieces      
# =======================

season_selector = checkboxGroupInput(
  'season_sel',
  'Season Selector',
  choices = c("Summer","Winter"),
  selected = c("Summer","Winter")
)

slider_factor_options = factor(x = c("Neither Summer nor Winter",
                                     "Summer or Winter",
                                     "Summer and Winter"))

streams_to_show = shinyWidgets::sliderTextInput(
  'streams_to_show',
  'Streams to Show',
  choices = slider_factor_options,
  selected = 'Summer and Winter'
)

sidebar = div(
  season_selector,
  streams_to_show,
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
  tags$div(
    id = "loadingImg",
    tags$img(id = "loadingImg", 
             src = "map_load_still_complete.png", 
             style = "position:absolute;width:145vh;
           height:90vh;top:10.5vh;left:60vh;z-index:1000;"),
    tags$div(
      style = 'position:absolute;width:145vh;background-color:grey;opacity:0.5;height:90vh;top:10.5vh;left:60vh;z-index:1100;'
    ),
    tags$p('Loading... please wait 1 - 2 minutes...',
           style = 'position:absolute;top:55vh;left:120vh;z-index:1101;font-family:fantasy;font-size:larger;')
  ),
    leafletOutput('leaflet_map',
                height = '90vh'
                )
)
    
ui <- page_navbar(
  shinyjs::useShinyjs(),
  theme = bslib::bs_theme(preset = 'flatly'),
  # title = 'Stream Drought Sandbox',
  title = 'Ptolemy Drought-Sensitive Stream Visualizer',
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
