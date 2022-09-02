# Little helper function for hover labels
create_labels <- function(data, map_level) {
  labels <- sprintf(
    "<strong>%s</strong><br/>Average temperature [in °C]: %g</sup>",
    data[, paste0("NAME_", map_level)], round(data$tavg, 1)
  )
  labels %>% lapply(htmltools::HTML)
}
