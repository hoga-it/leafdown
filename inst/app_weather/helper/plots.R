custom_theme <- theme(
  axis.text = element_text(size = 20, color = "white"),
  plot.title = element_text(margin = margin(0, 0, 20, 0)),
  title = element_text(size = 20, color = "white"),
  legend.text = element_text(size = 20, color = "white"),
  legend.title = element_text(size = 20),
  legend.background = element_rect(fill = "#343a40"),
  legend.key = element_rect(fill = "#343a40", color = "#343a40"),
  panel.background = element_rect(fill = "#343a40"),
  panel.grid = element_line(color = "black", size = 0.75),
  plot.background = element_rect(fill = "#343a40", color = "#343a40"),
  plot.title.position = "plot",
  plot.caption.position =  "plot"
)

create_line_plot <- function(curr_sel_data, curr_map_level, df_stations_monthly, active_markers) {

  if (curr_map_level == 1) {
    state_names <- curr_sel_data$NAME_1
    df <- df_stations_monthly %>% filter(State %in% state_names)
    var_name <- sym("State")
  } else if (curr_map_level == 2) {
    state_names <- curr_sel_data$NAME_1
    county_names <- curr_sel_data$NAME_2
    df <- df_stations_monthly %>% filter(State %in% state_names & County %in% county_names)
    var_name <- sym("County")
  } else {
    df <- df_stations_monthly %>% filter(station %in% active_markers$station)
    var_name <- sym("station")
  }

  ggplot(df) +
      geom_smooth(aes(x = time, y = tavg, color = !!var_name, fill = !!var_name), alpha = 0.1, se = FALSE) +
      theme_classic() +
      ggtitle("Development of the average temperature [°C]") +
      ylab("") +
      xlab("") +
      scale_fill_brewer(palette = "Set2") +
      scale_color_brewer(palette = "Set2") +
      scale_y_continuous(labels = scales::label_number(suffix = " °C", accuracy = 1), n.breaks = 4) +
      custom_theme

}

create_scatter_plot <- function(curr_sel_data, curr_map_level, df_stations_monthly, active_markers) {

  if (curr_map_level == 1) {
    state_names <- curr_sel_data$NAME_1
    df <- df_stations_monthly %>%
      filter(State %in% state_names) %>%
      group_by(State) %>%
      summarise(tavg = mean(tavg, na.rm = TRUE), prcp = mean(prcp, na.rm = TRUE))
    var_name <- sym("State")
  } else if (curr_map_level == 2) {
    county_names <- curr_sel_data$NAME_2
    state_names <- curr_sel_data$NAME_1
    df <- df_stations_monthly %>%
      filter(State %in% state_names & County %in% county_names) %>%
      group_by(County) %>%
      summarise(tavg = mean(tavg, na.rm = TRUE), prcp = mean(prcp, na.rm = TRUE))
    var_name <- sym("County")
  } else {
    df <- df_stations_monthly %>%
      filter(station %in% active_markers$station) %>%
      group_by(station) %>%
      summarise(tavg = mean(tavg, na.rm = TRUE), prcp = mean(prcp, na.rm = TRUE))
    var_name <- sym("station")
  }

  ggplot(df, aes(x = prcp, y = tavg)) +
    geom_point(aes(x = prcp, y = tavg, color = !!var_name), size = 10) +
    geom_label_repel(
      aes(label = !!var_name), fill = "#343a40", color = "white", label.size = NA, size = 8, point.padding = 20
    ) +
    theme_classic() +
    ggtitle("Average temperature [°C] and monthly precipitation [mm]") +
    ylab("") +
    xlab("") +
    scale_color_brewer(palette = "Set2") +
    scale_y_continuous(labels = scales::label_number(suffix = " °C", accuracy = 1), n.breaks = 4) +
    scale_x_continuous(labels = scales::label_number(suffix = " mm")) +
    custom_theme +
    theme(legend.position = "none")

}


