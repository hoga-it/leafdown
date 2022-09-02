update_leafdown_map <- function(my_leafdown, input, df_stations_monthly, sel_data_before_drilldown) {
  curr_data <- my_leafdown$curr_data
  curr_map_level <- my_leafdown$curr_map_level
  new_data <- curr_data

  if (curr_map_level == 1) {
    new_data$tavg <- avg_temp_per_state(df_stations_monthly, curr_data)
  } else {
    new_data$tavg <- avg_temp_per_county(df_stations_monthly, curr_data)
  }
  my_leafdown$add_data(new_data)

  draw_arg_list <- create_draw_arg_list(new_data, curr_map_level)

  my_leafdown$activate_shape_selection()
  map <- my_leafdown$draw_leafdown(
    fillColor = draw_arg_list$fill_color,
    weight = draw_arg_list$weight,
    fillOpacity = draw_arg_list$fill_opacity,
    color = "grey",
    label = draw_arg_list$labels,
    labelOptions = labelOptions(style = list("font-size" = "16px")),
    highlight = draw_arg_list$highlight
  ) %>%
    setView(-96, 39, 5) %>%
    my_leafdown$keep_zoom(input) %>%
    addLegend(
      pal = colorNumeric(color_ramp_blue_red, new_data$tavg, na.color = NA),
      values = new_data$tavg,
      title = "Average temperature",
      labFormat = labelFormat(suffix = "  °C"),
      opacity = 1,
      layerId = "legend"
    )

  if (curr_map_level == 3) {
    my_leafdown$deactivate_shape_selection()
    markers <- avg_temp_per_displ_marker(df_stations_monthly, sel_data_before_drilldown)
    map <- map %>%
      addGlPoints(
        data = st_as_sf(markers, coords = c("longitude", "latitude")),
        label = paste("Station: ", markers$station),
        fillOpacity = 1,
        radius = 30,
        fillColor = colorNumeric(color_ramp_blue_red, markers$tavg)(markers$tavg),
        layerId = "points"
      )
  }

  map

}

