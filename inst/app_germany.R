library(shiny)
library(leaflet)
library(leafdown)
library(dplyr)
library(readr)

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
  ger1 <- readRDS("extdata/gadm36_DEU_1_sp.rds")
  ger2 <- readRDS("extdata/gadm36_DEU_2_sp.rds")
  gpd_states <- read.csv2("extdata/gdp2014_germany_1.csv",
                           fileEncoding = "UTF-8", stringsAsFactors = FALSE)


  spdfs_list <- list(ger1, ger2)
  my_leafdown <- Leafdown$new(spdfs_list, "leafdown", input)

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

  output$leafdown <- renderLeaflet({
    req(spdfs_list)
    rv$update_leafdown
    data <- my_leafdown$get_current_data()
    if (my_leafdown$curr_map_level == 1) {
      data <- left_join(data, gpd_states, by = c("NAME_1" = "Federal_State"))
    } else {
      data$GDP_2014 <- 1:nrow(data)
    }

    my_leafdown$add_data(data)

    my_leafdown$draw_leafdown(
      fillColor = ~leaflet::colorNumeric("Greens", GDP_2014)(GDP_2014),
      weight = 2, fillOpacity = 0.7, color = "green"
    )
  })
}

shinyApp(ui, server)
