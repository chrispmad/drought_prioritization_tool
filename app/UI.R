library(shiny)
library(bslib)
library(leaflet)
library(leafpop)
library(capture)

# Record of Luke's stream digitization:

# 441 streams in Ron's original excel table. 
# That includes some that were Summer and Winter N/N sensitivity 
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
  choices = c("Summer","Winter","Summer and Winter"),
  selected = c("Summer and Winter")
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
  selected = '#eddd47'
)

stream_yy_picker = shinyWidgets::colorPickr(
  'stream_YY_colour',
  label = 'Summer and Winter',
  selected = 'red'
)

the_accordion = accordion(
  id = 'sidebar_accordion',
  multiple = FALSE,
  accordion_panel(
    title = 'Filters',
    nr_reg_selector,
    stream_by_name_selector
  ),
  accordion_panel(
    title = 'Visual Options',
    h5("Seasonal Drought Sensitivity"),
    # layout_column_wrap(
    #   1/3,
    season_selector,
    fluidRow(
      column(width = 4,
             stream_nn_picker
      ),
      column(width = 4,
             stream_some_picker),
      column(width = 4,
             stream_yy_picker)
    )
  ),
  accordion_panel(
    title = 'Data Downloads',
    h5("Ptolemy Data for Streams"),
    div(
      layout_column_wrap(
        1/2,
        # capture::capture(
        #   selector = "body",
        #   filename = paste0("Stream_Drought_Prioritization_",Sys.Date(),"_screenshot.png"),
        #   icon("camera"), "Capture",
        #   style = 'padding:2vh;display:grid;height:12vh;',
        #   class = "btn-info"
        # ),
        downloadButton('download_csv',shiny::HTML('Spreadsheet'),
                       style = 'padding:10px;display:grid;height:12vh;',
                       class = "btn-warning"
        ),
        downloadButton('download_gpkg','\n.GPKG',
                       style = 'padding:2vh;display:grid;height:12vh;',
                       class = "btn-secondary")
      ),
      style = 'padding-top:10px;'
    ),
    h5("Ptolemy Data for Ecosections (Summer + Winter)"),
    layout_column_wrap(
      1/2, 
      downloadButton('download_ecos_csv',shiny::HTML('Spreadsheet'),
                     style = 'padding:10px;display:grid;height:10vh;',
                     class = "btn-warning"
      ),
      downloadButton('download_ecos_gpkg','\n.GPKG',
                     style = 'padding:10px;display:grid;height:10vh;',
                     class = "btn-secondary")
    ),
    h5("Ptolemy Data for Water Survey\n Canada Hydrometric Stations"),
    layout_column_wrap(
      1/2,
      downloadButton('download_wsc_csv',shiny::HTML('Spreadsheet'),
                     style = 'padding:10px;display:grid;height:10vh;',
                     class = "btn-warning"
      ),
      downloadButton('download_wsc_gpkg','\n.GPKG',
                     style = 'padding:10px;display:grid;height:10vh;',
                     class = "btn-secondary")
    )
  )
)

sidebar = div(
  the_accordion,
  style = 'padding-top:10px;'
)

# Main page with map

main = div(
  leafletOutput('leaflet_map',
                height = '85vh'
  )
)

main_page = bslib::nav_panel(
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
)

metadata_page = bslib::nav_panel(
  title = 'Metadata',
  p("To learn more about how these metrics were calculated and dive into the methodology, \ndownload Ron Ptolemy's draft paper using the download button below."),
  downloadButton('download_EFN_paper',
                 "Download (~11 MB)",
                 style = 'padding:10px;display:grid;height:10vh;width:50vh;',
                 class = "btn-success"
  )
)

data_dictionary_page = bslib::nav_panel(
  title = 'Data Dictionary',
  bslib::card(
    DT::datatable(
      tidyr::tibble(
        STATION_NUMBER = 'Water Survey Canada Hydrometric Station Number',
        STATION_NAME = 'Water Survey Canada Hydrometric Station Name',
        location = 'Relative location in stream; possible values: lower, mid, upper',
        state = 'One of natural ("Nat") or "regulated" ("Reg"), i.e. the stream has point(s) of diversion/augmentation upstream of station',
        years_record = 'Number of years for which critical flow periods could be calculated',
        da_km_2 = 'Drainage Area in square kilometers',
        mad_l_s = 'Mean Annual Discharge in litres per second',
        runoff_l_s_km_2 = 'Annual runoff in litres per second per drainage area',
        mm_year = 'Annual runoff in millimeters per year',
        `20_mad` = '20% threshold of Mean Annual Discharge',
        alt_type = 'Alteration type: diversion (D), augmentation (A), or none (N)',
        summer_cpsf_mad = 'Percentage of Mean Annual Discharge during month with lowest average flows during Critical Period Stream Flows in Summer',
        max_summer = 'Maximum value during the CPSF summer period, as percent',
        min_summer = 'Minimum value during the CPSF summer period, as percent',
        winter_cpsf_mad = 'Percentage of Mean Annual Discharge during month with lowest average flows during Critical Period Stream Flows in Winter',
        max_winter = 'Maximum value during the CPSF winter period, as percent',
        min_winter = 'Minimum value during the CPSF winter period, as percent',
        lwy_fraction = 'low-water yield (measure of variability of annual run-off as fraction of long-term mean)',
        hwy_fraction = 'high-water yield (measure of variability of annual run-off as fraction of long-term mean)'
      ) |> 
        tidyr::pivot_longer(cols = dplyr::everything(),
                            names_to = 'Variable',
                            values_to = 'Defintion'),
      options = list(pageLength = 19)
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
  id = 'ptolemy_page',
  shinyjs::useShinyjs(),
  theme = bslib::bs_theme(preset = 'flatly'),
  title = 'Ptolemy Tool: Drought-Sensitive Stream Dataset',
  includeCSS('www/my_styles.css'),
  main_page,
  data_dictionary_page,
  metadata_page,
  nav_item(
    bslib::input_dark_mode()
  ),
  nav_item(
    capture::capture(
      selector = "body",
      filename = paste0("Stream_Drought_Prioritization_",Sys.Date(),"_screenshot.png"),
      span(icon("camera"), "Screenshot"),
      style = 'display:grid; height:5vh; width:18vh;',
      class = "btn-success"
    )
  ),
  nav_item(
    div(
      class = 'bc_logo'
    )
  )
)

ui
