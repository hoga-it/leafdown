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

  observeEvent(input$drill_down, {
    #TODO
  })

  observeEvent(input$drill_up, {
    #TODO
  })




  output$leafdown <- renderLeaflet({
    req(spdfs_list)
    my_leafdown <- Leafdown$new(spdfs_list, "leafdown", input)
    data <- my_leafdown$get_current_data()
    data$y <- 1:51
    my_leafdown$add_data(data)
    #print(data)
    labels <- sprintf(
      "<strong>%s</strong><br/>%g AA-Expertise / mi<sup>2</sup>",
      data$NAME_1, data$y
    ) %>% lapply(htmltools::HTML)

    my_leafdown$draw_leafdown(
      fillColor = ~leaflet::colorNumeric("Greens", y)(y),
      weight = 2,
      opacity = 1,
      color = "green",
      dashArray = "3",
      label = labels,
      fillOpacity = 0.7
    )
  })
}

shinyApp(ui, server)
