library(shiny)
library(bslib)
library(leaflet)
library(leafpop)
library(capture)

# Record of Luke's stream digitization:

# 441 streams in Ron's original excel table. 
# That includes some that were both N/N sensitivity 
# (not sure why they were included in the first place). 
# I think from the original unsuccessful list, 
# there were only 6 streams not yet accounted for that mattered 
# (9 total, but one was destroyed by a mine anyway, 
# one was Fort Nelson River but it was captured in another 
# ecosection anyway, and the other was an N/N sensitivity anyway
                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                    
# =======================
#   Read in data for inputs
# =======================

nr_regs = sf::read_sf('www/nr_regions.gpkg')

# =======================
#   Define UI pieces      
# =======================

## Join Ron's "Hydromaster" document, it's on OneDrive.
## It looks like this could be joined to water survey Canada 
## stations (like in Flowmaster 3000). Would the leaflet map
## be too busy? Maybe.

season_selector = radioButtons(
  'season_sel',
  'Season',
  choices = c("Summer","Winter","Both"),
  selected = c("Both")
)

stream_by_name_selector = fluidRow(
  column(
    width = 9,
    shinyWidgets::pickerInput(
      inputId = "stream_name_search",
      label = "Stream Name", 
      choices = NA,
      options = list(
        `live-search` = TRUE
      )
    )
  ),
  column(
    width = 2,
    actionButton(
      'reset_name_search',
      'Reset'
    )
  ),
  style = 'align-items:center;'
)

nr_reg_selector = shinyWidgets::pickerInput(
  inputId = "nr_reg_search",
  label = "Natural Resource Region", 
  choices = c('Whole Province',stringr::str_remove(nr_regs$REGION_NAME,' Natural.*')),
  selected = 'Whole Province',
  options = list(
    `live-search` = TRUE
  )
)

stream_geometry_simplification = sliderInput(
  'str_geo_simpl_amount',
  'Stream Geometry Simplification (m)',
  value = 50,
  min = 0, 
  max = 100, 
  step = 5
) |> 
  bslib::tooltip("Down-sample the number of points, merging all points in X meters")

stream_nn_picker = shinyWidgets::colorPickr(
  'stream_NN_colour',
  label = 'Neither',
  selected = 'darkblue'
)

stream_some_picker = shinyWidgets::colorPickr(
  'stream_some_colour',
  label = 'Summer or Winter',
  selected = 'yellow'
)

stream_yy_picker = shinyWidgets::colorPickr(
  'stream_YY_colour',
  label = 'Summer and Winter',
  selected = 'red'
)

the_accordion = accordion(
  id = 'sidebar_accordion',
  accordion_panel(
    title = 'Filters',
    season_selector,
    nr_reg_selector,
    stream_by_name_selector
  ),
  accordion_panel(
    title = 'Visual Options',
    h5("Seasonal Drought Sensitivity"),
    # layout_column_wrap(
    #   1/3,
    fluidRow(
      column(width = 4,
             stream_nn_picker
             ),
      column(width = 4,
             stream_some_picker),
      column(width = 4,
             stream_yy_picker)
    )#,
      # stream_nn_picker,
      # stream_some_picker,
      # stream_yy_picker,
    # ),
    # stream_geometry_simplification
  )
)

sidebar = div(
  the_accordion,
  div(
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

metadata_page = bslib::nav_panel(
      title = 'Metadata',
        bslib::card(
          card_image(
            file = 'www/Ron_Excel_Screenshot_clipped.png',
            width = '1000px'
          )
      )
    )

# Metadata page
# metadata_page = bslib::nav_panel(
#   title = 'Metadata',
#   h1("Data Sources"),
#   h3("BC Data Catalogue"),
#   h5("1. Conservation Data Center"),
#   p("Publically available Aquatic Species-at-risk layer"),
#   h5('2. Streams from the Freshwater Atlas layer'),
#   p("Stream network lines (only steams of order 3 or greater included)"),
#   h5('3. PSCIS (Provincial Stream Crossing Inventory System)'),
#   p('Describes various measures of fish passage, habitat quality, waterway morphology.'),
#   h3("Provincial Hydrologist Ron Ptolemy"),
#   h5('1. Drought Sensitivity of Ecosections'),
#   HTML("<br><br>"),
#   h3("Contact"),
#   h5("Luke Eilertsen"),
#   p('Luke.Eilertsen@gov.bc.ca'),
#   h5("Chris Madsen"),
#   p('Chris.Madsen@gov.bc.ca')
# )
    
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
  metadata_page,
  nav_item(
    bslib::input_dark_mode()
  )
)
