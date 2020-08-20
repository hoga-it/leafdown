library(shiny)
library(leaflet)
library(leafdown)
library(shinyjs)
# Define UI for leafdown app
ui <- shiny::fluidPage(
  useShinyjs(),
  # App title
  headerPanel("Drillable map with leafdown"),

  # Main
  actionButton("drill_down", "Drill Down"),
  actionButton("drill_up", "Drill Up"),
  actionButton("print", "Print"),
  leafletOutput("leafdown"),
  p(),
)

# Define server for leafdown app
server <- function(input, output) {
  states <- readRDS("testapps/us1-0005.RDS")
  states2 <- readRDS("testapps/us2-0005.RDS")

  #states <- readRDS("tests/testthat/testapps/us1-0005.RDS")
  #states2 <- readRDS("tests/testthat/testapps/us2-0005.RDS")

  spdfs_list <- list(states, states2)

  input <- reactiveValues(foo = "bar")
  map_id <- "leafdown"
  my_leafdown <- Leafdown$new(spdfs_list, map_id, input)
  data <- my_leafdown$get_current_metadata()
  data$y <- 1:nrow(data)
  my_leafdown$add_data(data)
  my_leafdown$draw_leafdown()


  my_leafdown <- leafdown::Leafdown$new(spdfs_list, "leafdown", input)

  rv <- reactiveValues()
  rv$update_leafdown <- 0

  observeEvent(input$drill_down, {
    my_leafdown$drill_down()
    rv$update_leafdown <- rv$update_leafdown + 1
  })

  observeEvent(input$drill_up, {
    my_leafdown$drill_up()
    rv$update_leafdown <- rv$update_leafdown + 1
  })

  observeEvent(input$print, {
    print(my_leafdown$curr_selection)
  })

  output$leafdown <- renderLeaflet({
    req(spdfs_list)
    rv$update_leafdown
    data <- my_leafdown$get_current_metadata()
    data$y <- 1:nrow(data)
    my_leafdown$add_data(data)
    my_leafdown$draw_leafdown(
      fillColor = ~leaflet::colorNumeric("Greens", y)(y),
      weight = 2, fillOpacity = 0.7, color = "green",
      highlight = highlightOptions()
    )
  })

  exportTestValues(my_leafdown = { my_leafdown })
}

shinyApp(ui, server)
