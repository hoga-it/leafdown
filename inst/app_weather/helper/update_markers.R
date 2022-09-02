update_markers <- function(active_marker_ids, markers) {
  colors <- colorNumeric(color_ramp_blue_red, markers$tavg)(markers$tavg)
  colors[active_marker_ids] <- "#FFFF00"
  radius <- rep(30, nrow(markers))
  radius[active_marker_ids] <- 40

  leafletProxy("leafdown") %>%
    removeGlPoints(layerId = "points") %>%
    addGlPoints(
      data = st_as_sf(markers, coords = c("longitude", "latitude")),
      popup = NULL,
      label = paste("Station: ", markers$station),
      fillOpacity = 1,
      radius = radius,
      fillColor = colors,
      layerId = "points"
    ) %>%
    removeControl("legend") %>%
    addLegend(
      "topright",
      pal = colorNumeric(color_ramp_blue_red, markers$tavg),
      values = markers$tavg,
      title = "Average temperature",
      labFormat = labelFormat(suffix = " °C"),
      opacity = 1,
      layerId = "legend"
    )
}
