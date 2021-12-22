library(leafdown)
# Run before uploading
#devtools::install_github("hoga-it/leafdown")

# Comment this when uploading
us0 <- readRDS("data/shapes/usa0.RDS")
us1 <- readRDS("data/shapes/usa1.RDS")
us2 <- readRDS("data/shapes/usa2.RDS")


# Define server for leafdown app
server <- function(input, output) {
  # load the shapes for the two levels
  spdfs_list <- list(us0, us1)

  # create leafdown object
  my_leafdown <- Leafdown$new(spdfs_list, "leafdown", input, join_map_levels_by = c("GID_0" = "GID_0"))

  rv <- reactiveValues()
  rv$update_leafdown <- 0

  # observers for the drilling buttons
  observeEvent(input$drill_down, {
    my_leafdown$drill_down()
    rv$update_leafdown <- rv$update_leafdown + 1
  })

  observeEvent(input$drill_up, {
    my_leafdown$drill_up()
    rv$update_leafdown <- rv$update_leafdown + 1
  })

  data <- reactive({
    req(rv$update_leafdown)
    # fetch the current metadata from the leafdown object
    data <- my_leafdown$curr_data

    data$y <- runif(nrow(data), 60, 100)
    # add the data back to the leafdown object
    my_leafdown$add_data(data)
    data
  })

  # this is where the leafdown magic happens
  output$leafdown <- renderLeaflet({
    req(spdfs_list)
    req(data)

    data <- data()
    # draw the leafdown object
    my_leafdown$draw_leafdown(
      fillColor = ~leaflet::colorNumeric("Greens", data$y)(data$y),
      weight = 3, fillOpacity = 1, color = "white") %>%
      # set the view to be center on the USA
      setView(-95, 39,  4)
  })
}
