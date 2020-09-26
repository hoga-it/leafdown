library(leafdown)
# Run this before uploading
# devtools::install_github("https://github.com/hoga-it/leafdown", ref = "pkgdown_website")
library(leaflet)
library(shiny)
library(dplyr)
library(shinycssloaders)
library(shinyjs)

# Uncomment this when uploading
ger1 <- readRDS("extdata/ger1-005.R")
ger2 <- readRDS("extdata/ger2-005.R")

# Comment this when uploading
#ger1 <- readRDS("../inst/extdata/ger1-005.R")
#ger2 <- readRDS("../inst/extdata/ger2-005.R")

ger2@data[c(76, 99, 136, 226), "NAME_2"] <- c(
  "Fürth (Kreisfreie Stadt)",
  "München (Kreisfreie Stadt)",
  "Osnabrück (Kreisfreie Stadt)",
  "Würzburg (Kreisfreie Stadt)"
)

spdfs_list <- list(ger1, ger2)

create_labels <- function(data, map_level) {
  labels <- sprintf(
    "<strong>%s</strong><br/>%g € per capita</sup>",
    data[, paste0("NAME_", map_level)], data$GDP_2014
  )
  labels %>% lapply(htmltools::HTML)
}

ui <- shiny::fluidPage(
  tags$style(HTML(".leaflet-container {background: #ffffff;}")),
  # Main
  useShinyjs(),
  actionButton("drill_down", "Drill Down"),
  actionButton("drill_up", "Drill Up"),
  withSpinner(leafletOutput("leafdown", height = 600), type = 8)
)

server <- function(input, output) {
  my_leafdown <- Leafdown$new(spdfs_list, "leafdown", input)
  update_leafdown <- reactiveVal(0)

  observeEvent(input$drill_down, {
    my_leafdown$drill_down()
    update_leafdown(update_leafdown() + 1)
  })

  observeEvent(input$drill_up, {
    my_leafdown$drill_up()
    update_leafdown(update_leafdown() + 1)
  })

  output$leafdown <- renderLeaflet({
    update_leafdown()
    data <- my_leafdown$get_current_metadata()
    curr_map_level <- my_leafdown$curr_map_level
    if (curr_map_level == 1) {
      data <- data %>% left_join(gdp_2014_federal_states, by = c("NAME_1" = "Federal_State"))
    } else {
      data <- data %>% left_join(gdp_2014_admin_districts, by = c("NAME_2" = "Admin_District"))
    }

    my_leafdown$add_data(data)
    labels <- create_labels(data, curr_map_level)
    my_leafdown$draw_leafdown(
      fillColor = ~ colorNumeric("Blues", GDP_2014)(GDP_2014),
      weight = 2, fillOpacity = 0.8, color = "grey", label = labels,
      highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.7)
    ) %>%
      addLegend("topright",
        pal = colorNumeric("Blues", data$GDP_2014),
        values = data$GDP_2014,
        title = "GDP per capita (2014)",
        labFormat = labelFormat(suffix = "€"),
        opacity = 1
      )
  })
}

shinyApp(ui, server)
