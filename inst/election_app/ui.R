library(shiny)
library(leaflet)
library(leafdown)
library(shinyjs)
library(bs4Dash)
library(echarts4r)
library(readr)

ui1 = bs4DashPage(
  title = "Leafdown Showcase - USA Election Data",
  navbar = bs4DashNavbar(tags$h3("Leafdown Showcase - USA Election Data", style = "margin-bottom: .2rem;")),
  bs4DashSidebar(disable = TRUE),
  body = bs4DashBody(
    # set the background of the map-container to be white
    tags$head(
      tags$style(HTML(".leaflet-container { background: #fff; }")),
      # workaround for the NA in leaflet legend see https://github.com/rstudio/leaflet/issues/615
      tags$style(HTML(".leaflet-control div:last-child {clear: both;}"))
    ),
    # we need shinyjs for the leafdown map
    useShinyjs(),
    fluidRow(
      column(
        width = 7,
        # a card for the map
        bs4Card(
          title = "Map",
          maximizable = T,
          closable = F,
          collapsible = F,
          width = 12,
          height = "100%",
          # the two buttons used for drilling
          actionButton("drill_down", "Drill Down"),
          actionButton("drill_up", "Drill Up"),
          # a dropdown to select what KPI should be displayed on the map
          selectInput("map_sel", "Select what KPI to display on the map:",
                      c("Votes" = "votes","Unemployment" = "unemployment")),
          # the actual map element
          leafletOutput("leafdown")
        )
      ),

      # column with the two graphs
      column(
        width = 5,
        # box for racial makeup graph
        bs4Box(
          width = 12,
          echarts4rOutput("socio")
        ),
        # box for party percent graph
        bs4Box(
          solidHeader = T,
          width = 12,
          echarts4rOutput("party")
        )
      )
    )
  )
)
