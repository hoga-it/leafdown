library(shiny)
library(leaflet)
library(leafdown)
library(shinyjs)
library(echarts4r)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# Define server for leafdown app
server <- function(input, output) {
  # load the shapes for the two levels
  states <- readRDS("../extdata/us1-0005.RDS")
  states2 <- readRDS("../extdata/us2-0005.RDS")
  spdfs_list <- list(states, states2)

  # load the election-data for the two levels
  usa16_1 <- read_csv("data/us16_1.csv")
  usa16_2 <- read_csv("data/us16_2.csv")

  # create leafdown object
  my_leafdown <- Leafdown$new(spdfs_list, "leafdown", input)
  #
  rv <- reactiveValues()
  rv$update_leafdown <- 0
  rv$df <- data.frame()
  rv$df_selected <- data.frame()

  shiny::observeEvent(input$leafdown_shape_click, {
    rv$df_selected <- my_leafdown$curr_sel_data
  })

  # observers for the drilling buttons
  observeEvent(input$drill_down, {
    my_leafdown$drill_down()
    rv$update_leafdown <- rv$update_leafdown + 1
  })

  observeEvent(input$drill_up, {
    my_leafdown$drill_up()
    rv$update_leafdown <- rv$update_leafdown + 1
  })

  # this is where the leafdown magic happens
  output$leafdown <- renderLeaflet({
    req(spdfs_list)
    req(rv$update_leafdown)
    # fetch the current metadata from the leafdown object
    data <- my_leafdown$get_current_metadata()

    # join the metadata with the election-data.
    # depending on the map_level we have different election-data so the 'by' columns for the join are different
    if(my_leafdown$curr_map_level == 2) {
      data$state_abbr <- substr(data$HASC_2, 4, 5)
      data <- left_join(data, usa16_2, by = c("NAME_2", "state_abbr"))
    } else {
      data$state_abbr <- substr(data$HASC_1, 4, 5)
      data <- left_join(data, usa16_1, by = "state_abbr")
    }

    # depending on the selected KPI in the dropdown we show different data
    if(input$map_sel == "unemployment") {
      data$y <- data$Unemployment * 100
      fillcolor <- leaflet::colorNumeric("Greens", data$y)
      legend_title <- "Unemployment in Percent"
    } else {
      data$y <- ifelse(data$Republicans2016 > data$Democrats2016, "Republicans", "Democrats")
      fillcolor <- leaflet::colorFactor(c("#232066", "#E91D0E"), data$y)
      legend_title <- "Winning Party"
    }

    # add the data back to the leafdown object
    my_leafdown$add_data(data)

    # draw the leafdown object
    my_leafdown$draw_leafdown(
      fillColor = ~fillcolor(data$y),
      weight = 2, fillOpacity = 0.7, color = "black") %>%
      # set the view to be center on the USA
      setView(-95, 39,  4)  %>%
      # add a nice legend
      addLegend(pal = fillcolor,
                values = ~data$y,
                title = legend_title,
                opacity = 1)
  })

  # plots
  output$socio <- renderEcharts4r({
    req(rv$df_selected)
    # check whether any shape is selected, show basic info for the whole usa if nothing is selected
    if(dim(rv$df_selected)[1] > 0){
      if(my_leafdown$curr_map_level == 1) {
        df <- rv$df_selected[, c("state_abbr", "Hispanic", "White", "Black", "Asian", "Amerindian", "Other")]
        df <- df %>% pivot_longer(2:7, "race") %>% group_by(state_abbr)
        df$value <- round(df$value * 100, 2)
      } else {
        df <- rv$df_selected[, c("County", "Hispanic", "White", "Black", "Asian", "Amerindian", "Other")]
        df <- df %>% pivot_longer(2:7, "race") %>% group_by(County)
      }
    } else {
      # show basic info for the whole usa as no state is selected
      df <- data.frame(
        state_abbr = "USA",
        race = c("Hispanic", "White", "Black", "Asian", "Amerindian", "Other"),
        value = c(15.3, 63.4, 13.4, 5.9, 1.5, 2.7)) %>% group_by(state_abbr)
    }
    # create the graph
    df %>%
      e_charts(race) %>%
      e_bar(value) %>%
      e_tooltip(trigger = "axis",axisPointer = list(type = "shadow")) %>%
      e_title(text = "Racial makeup in percentages") %>%
      e_y_axis(splitArea = list(show = FALSE),splitLine = list(show = FALSE)) %>%
      e_legend(orient = 'vertical',right = 10,top = 10) %>%
      e_color(brewer.pal(nrow(df), "Set3"))
  })

  output$party <- renderEcharts4r({
    req(rv$df_selected)
    # check whether any shape is selected, show general election-result if nothing is selected
    if(dim(rv$df_selected)[1] > 0){
      if(my_leafdown$curr_map_level == 1) {
        df <- rv$df_selected[, c("state_abbr", "Democrats2016", "Republicans2016", "Libertarians2016", "Green2016")]
        df <- df %>% pivot_longer(2:5, "party") %>% group_by(party)
      } else {
        df <- rv$df_selected[, c("County", "Democrats2016", "Republicans2016", "Libertarians2016", "Green2016")]
        df <- df %>% pivot_longer(2:5, "party") %>% group_by(party)
        df$value <- df$value / 100
        names(df)[1] <- "state_abbr"
      }
    } else {
      # show general election-result as no state is selected
      df <- data.frame(
        party = c("Democrats2016", "Republicans2016", "Libertarians2016", "Green2016"),
        state_abbr = "USA",
        value = c(0.153, 0.634, 0.134, 0.059)) %>% group_by(party)
    }
    # create the graph
    df %>%
      e_charts(state_abbr, stack="grp") %>%
      e_bar(value) %>%
      e_y_axis(formatter = e_axis_formatter("percent", digits = 2)) %>%
      e_tooltip(trigger = "axis",axisPointer = list(type = "shadow")) %>%
      e_title(text = "Votes in percent") %>%
      e_legend(right = 10,top = 10) %>%
      e_color(c("#232066", "#E91D0E", "#f3b300", "#006900"))
  })
}
