library(leafdown)
# Run this before uploading
# devtools::install_github("https://github.com/hoga-it/leafdown/tree/multiple-map-levels")
library(leaflet)
library(shiny)
library(dplyr)
library(shinycssloaders)
library(shinyjs)
library(dplyr)

# Uncomment this when uploading
# ger0 <- readRDS("ger0-005.RDS")
# ger1 <- readRDS("ger1-005.RDS")
# ger2 <- readRDS("ger2-005.RDS")
# aut0 <- readRDS("a0-005.RDS")
# aut1 <- readRDS("a1-005.RDS")
# aut2 <- readRDS("a2-005.RDS")
# ch0 <- readRDS("ch0-005.RDS")
# ch1 <- readRDS("ch1-005.RDS")
# ch2 <- readRDS("ch2-005.RDS")


# Comment this when uploading
ger0 <- readRDS("../extdata/ger0-005.RDS")
ger1 <- readRDS("../extdata/ger1-005.RDS")
ger2 <- readRDS("../extdata/ger2-005.RDS")
aut0 <- readRDS("../extdata/a0-005.RDS")
aut1 <- readRDS("../extdata/a1-005.RDS")
aut2 <- readRDS("../extdata/a2-005.RDS")
ch0 <- readRDS("../extdata/ch0-005.RDS")
ch1 <- readRDS("../extdata/ch1-005.RDS")
ch2 <- readRDS("../extdata/ch2-005.RDS")

# load the shapes for the three levels
spdfs_list <- list(rbind(aut0, ch0, ger0), rbind(aut1, ch1, ger1), rbind(aut2, ch2, ger2))

set.seed(20220106)
data_sim_y_level_3 <- spdfs_list[[3]]@data
data_sim_y_level_3$y <- rnorm(nrow(data_sim_y_level_3), 1e2, sd = 5e2)
data_sim_y_level_2 <- data_sim_y_level_3 %>% group_by(NAME_0, NAME_1) %>% summarise(y = sum(y))
data_sim_y_level_1 <- data_sim_y_level_2 %>% group_by(NAME_0) %>% summarise(y = sum(y))

data_sim_y_level_3$level <- 3
data_sim_y_level_2$level <- 2
data_sim_y_level_1$level <- 1

data_sim_y_level_3$area <- data_sim_y_level_3$NAME_2
data_sim_y_level_2$area <- data_sim_y_level_2$NAME_1
data_sim_y_level_1$area <- data_sim_y_level_1$NAME_0

data_sim_y <- rbind(
  data_sim_y_level_3[, c("area", "y", "level")],
  data_sim_y_level_2[, c("area", "y", "level")],
  data_sim_y_level_1[, c("area", "y", "level")]
)
data_sim_y$y <- round(data_sim_y$y, 0)

create_labels <- function(data, map_level) {
  labels <- sprintf(
    "<strong>%s</strong><br/>%g</sup>",
    data[, paste0("NAME_", map_level - 1)], data$y
  )
  labels %>% lapply(htmltools::HTML)
}

ui <- fluidPage(
  mainPanel(
    # set the background of the map-container to be white
    tags$head(
      tags$style(HTML(".leaflet-container { background: #fff; height: 100%}")),
      # workaround for the NA in leaflet legend see https://github.com/rstudio/leaflet/issues/615
      tags$style(HTML(".leaflet-control div:last-child {clear: both;}"))
    ),
    # we need shinyjs for the leafdown map
    useShinyjs(),
    fluidRow(
      # the two buttons used for drilling
      actionButton("drill_down", "Drill Down"),
      actionButton("drill_up", "Drill Up"),
      # the actual map element
      withSpinner(leafletOutput("leafdown", height = 800), type = 8)
    )
  )
)



# Define server for leafdown app
server <- function(input, output) {


  # create leafdown object
  my_leafdown <- Leafdown$new(spdfs_list, "leafdown", input, join_map_levels_by = c("GID_0" = "GID_0", "GID_1" = "GID_1"))

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
    meta_data <- my_leafdown$curr_data
    curr_map_level <- my_leafdown$curr_map_level
    data_curr_map_level <- data_sim_y[data_sim_y$level == curr_map_level, ]
    join_col_lhs <- paste0("NAME_", curr_map_level - 1)
    data <- meta_data %>% left_join(data_curr_map_level, by = setNames("area", join_col_lhs))

    # add the data back to the leafdown object
    my_leafdown$add_data(data)
    data
  })

  # this is where the leafdown magic happens
  output$leafdown <- renderLeaflet({
    req(spdfs_list)
    req(data)

    data <- data()
    labels <- create_labels(data, my_leafdown$curr_map_level)
    # draw the leafdown object
    map <- my_leafdown$draw_leafdown(
      fillColor = ~leaflet::colorNumeric("Greens", data$y)(data$y),
      weight = 3, fillOpacity = 1, color = "grey", label = labels
    ) %>% my_leafdown$keep_zoom(input)

    map
  })
}

shinyApp(ui, server)

