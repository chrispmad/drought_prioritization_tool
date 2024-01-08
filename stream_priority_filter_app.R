library(shiny)
library(leaflet)
library(bslib)
library(sf)
library(bcmaps)

left_column = div(
  h5('MATH'),
  sliderInput('a','A',min = 0, max = 10, value = 2),
  sliderInput('b','B',min = 0, max = 10, value = 2),
  sliderInput('c','C',min = 0, max = 10, value = 2)
)

right_column = div(
  leafletOutput('ron_ecosections'),
  plotOutput('high_priority_stream_plot')
)

ui <- page_fluid(
  layout_column_wrap(
    1/2,
    left_column,
    right_column
  )
)

server <- function(input, output, session) {
  
  bc = bc_bound()
  
  
  output$ron_ecosections = renderLeaflet({
    leaflet() |> 
      addTiles()
  })
  
  output$high_priority_stream_plot = renderPlot({
    ggplot() + 
      geom_sf(data = bc)
  })
}

shinyApp(ui, server)