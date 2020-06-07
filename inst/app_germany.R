library(shiny)
library(leaflet)
library(leafdown)
library(dplyr)
library(readr)
library(stringr)

create_labels <- function(data, map_level) {
  labels <- sprintf(
    "<strong>%s</strong><br/>%g € per capita</sup>",
    data[, paste0("NAME_", map_level)], data$GDP_2014
  )
  labels %>% lapply(htmltools::HTML)
}

# Load data and spdfs ----------------------------------------------------------
ger1 <- readRDS("extdata/gadm36_DEU_1_sp.rds")
ger2 <- readRDS("extdata/gadm36_DEU_2_sp.rds")
# Correct Umlaute
ger2@data[c(76, 99, 136, 226), "NAME_2"] <- c(
  "Fürth (Kreisfreie Stadt)",
  "München (Kreisfreie Stadt)",
  "Osnabrück (Kreisfreie Stadt)",
  "Würzburg (Kreisfreie Stadt)"
)
spdfs_list <- list(ger1, ger2)
gpd_states <- read.csv2("extdata/gdp2014_germany_1.csv",
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)
gpd_admin_districts <- read.csv2("extdata/gdp2014_germany_2.csv",
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)
gpd_admin_districts$Admin_District <- str_replace(
  gpd_admin_districts$Admin_District,
  ", Stadt", ""
)


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
    curr_map_level <- my_leafdown$curr_map_level
    if (curr_map_level == 1) {
      data <- data %>% left_join(gpd_states, by = c("NAME_1" = "Federal_State"))
    } else {
      data <- data %>% left_join(gpd_admin_districts, by = c("NAME_2" = "Admin_District"))
    }
    my_leafdown$add_data(data)
    labels <- create_labels(data, curr_map_level)
    map <- my_leafdown$draw_leafdown(
      fillColor = ~ leaflet::colorNumeric("Blues", GDP_2014)(GDP_2014),
      weight = 2, fillOpacity = 0.8, color = "grey", label = labels
    )
    map <- map %>%
      addLegend("bottomright", pal = leaflet::colorNumeric("Blues", data$GDP_2014),
        values = ~GDP_2014, title = "GDP per capita (2014)",
        labFormat = labelFormat(prefix = "€"), opacity = 1
      )
  })
}

shinyApp(ui, server)



# library(readxl)
# gdp_admin_districts <- read_excel("inst/extdata/BIP_Landkreise.xlsx")
# gdp_admin_districts <- gdp_admin_districts[,c("name", "BIP_14")]
# head(gdp_admin_districts)
# ger2 <- readRDS("inst/extdata/gadm36_DEU_2_sp.rds")
# # Correct missing Umlaute
# ger2@data[c(76, 99, 136, 226), "NAME_2"] <- c(
#   "Fürth (Kreisfreie Stadt)",
#   "München (Kreisfreie Stadt)",
#   "Osnabrück (Kreisfreie Stadt)",
#   "Würzburg (Kreisfreie Stadt)"
# )
# head(ger2@data)
#
# library(stringr)
#
# gdp_admin_districts2 <- gdp_admin_districts
# gdp_admin_districts2$name <- gdp_admin_districts2$name %>% str_replace(", Stadt", "")
#
# data <- inner_join(ger2@data, gdp_admin_districts2, by = c("NAME_2" = "name"))
#
# data2 <- anti_join(ger2@data, gdp_admin_districts2, by = c("NAME_2" = "name"))
#
# data2$NAME_2
#
# gdp_admin_districts2 <- as.data.frame(gdp_admin_districts)
# names(gdp_admin_districts2) <- c("Admin_District", "GDP_2014")
#
# write.csv2(gdp_admin_districts2, file = "inst/extdata/gdp2014_germany_2.csv",
#            fileEncoding = "UTF-8", row.names = FALSE)
#
# gdp_admin_districts3 <- read.csv2("inst/extdata/gdp2014_germany_2.csv",
#                                  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
#
# gdp_admin_districts3
