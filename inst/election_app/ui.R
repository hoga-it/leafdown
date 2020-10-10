library(bs4Dash)
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
      tags$style(HTML(".leaflet-control div:last-child {clear: both;}")),
      tags$style(HTML(".card {height: 100%;}")),
      tags$style(HTML(".col-sm-12:last-child .card {margin-bottom: 0 !important;}")),
      tags$style(HTML("#leafdown {height: 80% !important; margin-top: 10px; margin-bottom: 10px;}"))
    ),
    # we need shinyjs for the leafdown map
    useShinyjs(),
    fluidRow(
        # a card for the map
        bs4Card(
          title = "Map",
          closable = F,
          collapsible = F,
          width = 7,
          # a dropdown to select what KPI should be displayed on the map
          selectInput("map_sel", "Select what KPI to display on the map:",
                      c("Votes" = "votes","Unemployment" = "unemployment")),
          # the two buttons used for drilling
          actionButton("drill_down", "Drill Down"),
          actionButton("drill_up", "Drill Up"),
          # the actual map element
          leafletOutput("leafdown")
      ),

      # a column with the two graphs
      column(
        width = 5,
        # box for racial makeup graph
        bs4Card(
          width = 12,
          closable = F,
          collapsible = F,
          title = "Racial makeup in percentages",
          echarts4rOutput("socio")
        ),
        # box for party percent graph
        bs4Card(
          width = 12,
          closable = F,
          collapsible = F,
          title = "Votes in percent",
          echarts4rOutput("party")
        )
      )
    )
  )
)
