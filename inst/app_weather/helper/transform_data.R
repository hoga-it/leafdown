avg_temp_per_displ_marker <- function(df_stations_monthly, curr_sel_data) {
  df_stations_monthly %>%
    semi_join(curr_sel_data, by = c("State" = "NAME_1", "County" = "NAME_2")) %>%
    group_by(station, latitude, longitude) %>%
    summarise(tavg = mean(tavg, na.rm = TRUE))
}

avg_temp_per_state <- function(df_stations_monthly, curr_data) {
  curr_data %>%
    left_join(df_stations_monthly, by = c("NAME_1" = "State")) %>%
    group_by(NAME_1) %>%
    summarise(tavg = mean(tavg, na.rm = TRUE)) %>%
    pull(tavg)
}

avg_temp_per_county <- function(df_stations_monthly, curr_data) {
  curr_data %>%
    left_join(df_stations_monthly, by = c("NAME_1" = "State", "NAME_2" = "County")) %>%
    group_by(NAME_1, NAME_2) %>%
    summarise(tavg = mean(tavg, na.rm = TRUE)) %>%
    pull(tavg)
}



