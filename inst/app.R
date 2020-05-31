library(shiny)
library(leaflet)
library(leafdown)
# Define UI for leafdown app
ui <- shiny::fluidPage(

  # App title
  headerPanel("Drillable map with leafdown"),

  # Main
  actionButton("drill_down", "Drill Down"),
  actionButton("drill_up", "Drill Up"),
  leafletOutput("leafdown"),
  p(),
)

# Define server for leafdown app
server <- function(input, output) {
  states <- readRDS("extdata/us1-0005.RDS")
  states2 <- readRDS("extdata/states2.RDS")
  spdfs_list <- list(states, states2)
  my_leafdown <- Leafdown$new(spdfs_list, "leafdown", input)

  rv <- reactiveValues()
  rv$update_leafdown <- 0
  observeEvent(input$drill_down, {
    my_leafdown$drill_down()
    rv$update_leafdown <- rv$update_leafdown + 1
  })

  observeEvent(input$drill_up, {
    #TODO
  })


  output$leafdown <- renderLeaflet({
    req(spdfs_list)
    rv$update_leafdown
    data <- my_leafdown$get_current_data()
    data$y <- 1:nrow(data)
    my_leafdown$add_data(data)
    my_leafdown$draw_leafdown(
      fillColor = ~leaflet::colorNumeric("Greens", y)(y),
      weight = 2, fillOpacity = 0.7, color = "green"
    )
  })
}

shinyApp(ui, server)
