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
  us0 <- readRDS("../res/usa0.RDS")
  us1 <- readRDS("../res/usa1.RDS")
  us2 <- readRDS("../res/usa2.RDS")
  ger0 <- readRDS("../res/ger0-005.RDS")
  ger1 <- readRDS("../res/ger1-005.RDS")
  ger2 <- readRDS("../res/ger2-005.RDS")

  spdfs_list <- list(raster::union(us0, ger0), raster::union(us1, ger1), raster::union(us2, ger2))

  my_leafdown <- Leafdown$new(spdfs_list, "leafdown", input, join_map_levels_by = c("GID_0" = "GID_0", "GID_1" = "GID_1"))
  eval_draw <- NULL
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
    print(my_leafdown$curr_sel_data)
  })

  output$leafdown <- renderLeaflet({
    req(spdfs_list)
    rv$update_leafdown
    data <- my_leafdown$curr_data
    data$y <- 1:nrow(data)
    my_leafdown$add_data(data)
    # input$args_leaflet is used for testing arguments in $draw_leafdown and
    # explicitly defined in the specific test
    if (!is.null(input[["args_leaflet"]])) {
      # potential warning message from $draw_leafdown
      eval_draw <<- testthat::capture_warning({
        do.call(my_leafdown$draw_leafdown, input$args_leaflet)
      })
    }
    my_leafdown$draw_leafdown()
  })

  exportTestValues(my_leafdown = { my_leafdown }, eval_draw = {eval_draw})
}

shinyApp(ui, server)
