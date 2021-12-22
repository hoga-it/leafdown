library(bs4Dash)
library(shiny)
library(shinyjs)
library(leaflet)
library(leafdown)
library(echarts4r)
library(dplyr)
library(tidyr)
library(RColorBrewer)

ui = bs4DashPage(
  title = "Leafdown Showcase - USA Election Data",
  navbar = bs4DashNavbar(tags$h3("Leafdown Showcase - USA Election Data", style = "margin-bottom: .2rem;")),
  bs4DashSidebar(disable = TRUE),
  body = bs4DashBody(
    # set the background of the map-container to be white
    tags$head(
      tags$style(HTML(".leaflet-container { background: #fff; height: 100%}")),
      # workaround for the NA in leaflet legend see https://github.com/rstudio/leaflet/issues/615
      tags$style(HTML(".leaflet-control div:last-child {clear: both;}"))
    ),
    # we need shinyjs for the leafdown map
    useShinyjs(),
    fluidRow(
        # a card for the map
        bs4Card(
          title = "Map",
          closable = F,
          collapsible = F,
          width = 12,
          height = "500px",
          # the two buttons used for drilling
          actionButton("drill_down", "Drill Down"),
          actionButton("drill_up", "Drill Up"),
          # the actual map element
          leafletOutput("leafdown")
      )
    )
  )
)
