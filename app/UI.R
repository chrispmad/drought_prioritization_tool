library(shiny)
library(bslib)
library(leaflet)
library(leafpop)
library(capture)

# 

# =======================
#   Define UI pieces      
# =======================

# Weights selection for variables.
winter_sens_w_input = div(
  fluidRow(
    column(
      width = 2,
      # Checkbox input
      checkboxInput('include_winter_sens',
                    '',
                    value = T)
    ),
    column(
      width = 10,
      numericInput('winter_sens_w_input',
                   'Winter',
                   min = 1, 
                   max = 5,
                   value = 1) |> 
        tooltip('a value from 1 to 5')
    )
  )
)

summer_sens_w_input = div(
  fluidRow(
    column(
      width = 2,
      # Checkbox input
      checkboxInput('include_summer_sens',
                    '',
                    value = T)
    ),
    column(
      width = 10,
      numericInput('summer_sens_w_input',
                   'Summer',
                   min = 1, 
                   max = 5,
                   value = 1) |> 
        tooltip('a value from 1 to 5')
    )
  )
)

winter_sens_exp_id_w_input = div(
  fluidRow(
    column(
      width = 2,
      # Checkbox input
      checkboxInput('include_exp_id_winter_sens',
                    '',
                    value = T)
    ),
    column(
      width = 10,
      numericInput('exp_id_winter_sens_w_input',
                   'Expert ID Winter',
                   min = 1, 
                   max = 5,
                   value = 1) |> 
        tooltip('a value from 1 to 5')
    )
  )
)

summer_sens_exp_id_w_input = div(
  fluidRow(
    column(
      width = 2,
      # Checkbox input
      checkboxInput('include_exp_id_summer_sens',
                    '',
                    value = T)
    ),
    column(
      width = 10,
      numericInput('exp_id_summer_sens_w_input',
                   'Expert ID Summer',
                   min = 1, 
                   max = 5,
                   value = 1) |> 
        tooltip('a value from 1 to 5')
    )
  )
)

habitat_quality_w_input = div(
  fluidRow(
    column(
      width = 2,
      # Checkbox input
      checkboxInput('include_habitat_quality',
                    '',
                    value = T)
    ),
    column(
      width = 10,
      numericInput('habitat_quality_w_input',
                   'Habitat Quality',
                   min = 1, 
                   max = 5,
                   value = 1) |> 
        tooltip('a value from 1 to 5')
    )
  )
)

sar_w_input = div(
  fluidRow(
    column(
      width = 2,
      # Checkbox input
      checkboxInput('include_sar',
                    '',
                    value = T)
    ),
    column(
      width = 10,
      numericInput('sar_w_input',
                   'Aquatic SAR',
                   min = 1, 
                   max = 5,
                   value = 1) |> 
        tooltip('SAR = Species-at-risk; a value from 1 to 5')
    )
  )
)

fish_obs_w_input = div(
  fluidRow(
    column(
      width = 2,
      # Checkbox input
      checkboxInput('include_fish_observed',
                    '',
                    value = T)
    ),
    column(
      width = 10,
      numericInput('fish_obs_w_input',
                   'Fish Observed',
                   min = 1, 
                   max = 5,
                   value = 1) |> 
        tooltip('a value from 1 to 5')
    )
  )
)

number_streams_to_show = sliderInput(
  'number_streams_to_show',
  'Number Streams to Show',
  min = 25,
  max = 300,
  value = 200,
  step = 25
)

weight_inputs = div(
  h5("Drought Sensitivity"),
  fluidRow(
    column(width = 6,
           winter_sens_w_input
           ),
    column(width = 6,
           summer_sens_w_input
           )
  ),
  fluidRow(
    column(width = 6,
           winter_sens_exp_id_w_input
    ),
    column(width = 6,
           summer_sens_exp_id_w_input
    )
  ),
  fluidRow(
    column(width = 6,
           sar_w_input
    ),
    column(width = 6,
           habitat_quality_w_input
    )
  ),
  fluidRow(
    column(width = 6,
           fish_obs_w_input
    ),
    column(width = 6
    )
  ),
  style = 'margin-top:-1rem;'
)

run_model_button = div(
  actionButton(
    'run_model_button',
    label = 'Rerun Model',
    icon = shiny::icon("repeat"),
    class = "btn-success"
  ),
  style = 'text-align:center;margin-top:0rem;margin-bottom:1rem;'
)

# Draggable toolbar
sidebar = #absolutePanel(
  #draggable = TRUE,
  # height = 500,
  # width = 400,
  # left = 80,
  # top = 60,
  # bslib::sidebar(
    # width = '30%',
  # card(
  div(
  number_streams_to_show,
  weight_inputs,
  run_model_button,
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
# ),
# style = 'z-index: 100;'


# Main page with map

main = div(
  leafletOutput('leaflet_map',
                height = '90vh'
                )
)
    
ui <- page_navbar(
  theme = bslib::bs_theme(preset = 'flatly'),
  title = 'Stream Drought Sensitivity',
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
    p("Stream network lines"),
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
