get_data <- function() {
  urlfile <- "https://raw.githubusercontent.com/andreash0/meteostat_monthly_measurements/master/meteostat_monthly.csv"

  suppressWarnings({
    read_csv(urlfile, col_types = c("station" = "c")) %>%
      mutate(
        time = as.Date(time),
        station = as.character(station)
      )
  })
}

color_ramp_blue_red <- colorRamp(c("#1984c5", "#e2e2e2", "#c23728"), interpolate = "linear")

create_draw_arg_list <- function(new_data, curr_map_level) {
  draw_arg_list <- list()
  if (curr_map_level != 3) {
    draw_arg_list$labels <- create_labels(new_data, curr_map_level)
    draw_arg_list$fill_color <- ~ colorNumeric(color_ramp_blue_red, tavg)(tavg)
    draw_arg_list$fill_opacity <- 1
    draw_arg_list$highlight <- highlightOptions(weight = 6, color = "#666", fillOpacity = 0.7)
    draw_arg_list$weight <- 2
  } else {
    draw_arg_list$labels <- NULL
    draw_arg_list$fill_color <- "#EDDCDC"
    draw_arg_list$fill_opacity <- 0.2
    draw_arg_list$highlight <- NULL
    draw_arg_list$weight <- 1
  }
  draw_arg_list
}
