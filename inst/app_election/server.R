library(leafdown)
# Run before uploading
#devtools::install_github("hoga-it/leafdown")

# Comment this when uploading
states <- readRDS("us1.RDS")
counties <- readRDS("us2.RDS")

percent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

create_labels <- function(data, map_level) {
  print(names(data))
  labels <- sprintf(
    "<strong>%s</strong><br/>
    Democrats: %s<br/>
    Republicans: %s<br/>
    Libertarians: %s<br/>
    Green: %s<br/>
    </sup>",
    data[, paste0("NAME_", map_level)],
    percent(data$Democrats2016),
    percent(data$Republicans2016),
    percent(data$Libertarians2016),
    percent(data$Green2016)
  )
  labels %>% lapply(htmltools::HTML)
}

# Define server for leafdown app
server <- function(input, output) {
  # load the shapes for the two levels
  spdfs_list <- list(states, counties)

  # create leafdown object
  my_leafdown <- Leafdown$new(spdfs_list, "leafdown", input)

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

    # join the metadata with the election-data.
    # depending on the map_level we have different election-data so the 'by' columns for the join are different
    if(my_leafdown$curr_map_level == 2) {
      data$ST <- substr(data$HASC_2, 4, 5)
      # there are counties with the same name in different states so we have to join on both
      data <- left_join(data, us_election_counties, by = c("NAME_2", "ST"))
    } else {
      data$ST <- substr(data$HASC_1, 4, 5)
      data <- left_join(data, us_election_states, by = "ST")
    }
    # add the data back to the leafdown object
    my_leafdown$add_data(data)
    data
  })

  # this is where the leafdown magic happens
  output$leafdown <- renderLeaflet({
    req(spdfs_list)
    req(data)

    data <- data()

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

    labels <- create_labels(data, my_leafdown$curr_map_level)
    # draw the leafdown object
    my_leafdown$draw_leafdown(
      fillColor = ~fillcolor(data$y),
      weight = 3, fillOpacity = 1, color = "white", label = labels) %>%
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
    df <- my_leafdown$curr_sel_data()
    # check whether any shape is selected, show basic info for the whole usa if nothing is selected
    if(dim(df)[1] > 0){
      if(my_leafdown$curr_map_level == 1) {
        df <- df[, c("State", "Hispanic", "White", "Black", "Asian", "Amerindian", "Other")]
        df <- df %>% pivot_longer(2:7, "race") %>% group_by(State)
        df$value <- round(df$value, 2)
      } else {
        df <- df[, c("County", "Hispanic", "White", "Black", "Asian", "Amerindian", "Other")]
        df <- df %>% pivot_longer(2:7, "race") %>% group_by(County)
        df$value <- round(df$value/100, 2)
      }
    } else {
      # show basic info for the whole usa as no state is selected
      df <- data.frame(
        ST = "USA",
        race = c("Hispanic", "White", "Black", "Asian", "Amerindian", "Other"),
        value = c(0.15, 0.634, 0.134, 0.059, 0.015, 0.027)) %>% group_by(ST)
    }
    # create the graph
    df %>%
      e_charts(race) %>%
      e_bar(value) %>%
      e_tooltip(trigger = "axis",axisPointer = list(type = "shadow")) %>%
      e_y_axis(splitArea = list(show = FALSE),
               splitLine = list(show = FALSE),
               formatter = e_axis_formatter("percent", digits = 2)) %>%
      e_legend(orient = 'vertical',right = 10,top = 10) %>%
      e_color(brewer.pal(nrow(df), "Set3")) %>%
      e_tooltip(formatter = e_tooltip_item_formatter("percent"))
  })

  output$party <- renderEcharts4r({
    df <- my_leafdown$curr_sel_data()
    # check whether any shape is selected, show general election-result if nothing is selected
    if(dim(df)[1] > 0){
      if(my_leafdown$curr_map_level == 1) {
        df <- df[, c("ST", "Democrats2016", "Republicans2016", "Libertarians2016", "Green2016")]
        df <- df %>% pivot_longer(2:5, "party") %>% group_by(party)
      } else {
        df <- df[, c("County", "Democrats2016", "Republicans2016", "Libertarians2016", "Green2016")]
        df <- df %>% pivot_longer(2:5, "party") %>% group_by(party)
        df$value <- df$value
        names(df)[1] <- "ST"
      }
    } else {
      # show general election-result as no state is selected
      df <- data.frame(
        party = c("Democrats2016", "Republicans2016", "Libertarians2016", "Green2016"),
        ST = "USA",
        value = c(0.153, 0.634, 0.134, 0.059)) %>% group_by(party)
    }
    # create the graph
    df %>%
      e_charts(ST, stack="grp") %>%
      e_bar(value) %>%
      e_y_axis(formatter = e_axis_formatter("percent", digits = 2)) %>%
      e_tooltip(trigger = "axis",axisPointer = list(type = "shadow")) %>%
      e_legend(right = 10,top = 10) %>%
      e_color(c("#232066", "#E91D0E", "#f3b300", "#006900"))%>%
      e_tooltip(formatter = e_tooltip_item_formatter("percent", digits = 2))
  })
}
