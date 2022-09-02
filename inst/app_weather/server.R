
server <- function(input, output) {
  my_leafdown <- Leafdown$new(spdfs_list, "leafdown", input)
  active_marker_ids <- NULL
  markers <- NULL

  rv_update_leafdown <- reactiveVal(0)
  rv_active_markers <- reactiveVal()

  observeEvent(input$drill_down, {
    my_leafdown$drill_down()
    rv_update_leafdown(rv_update_leafdown() + 1)
  })

  observeEvent(input$drill_up, {
    my_leafdown$drill_up()
    active_marker_ids <<- c()
    isolate(rv_active_markers(c()))
    rv_update_leafdown(rv_update_leafdown() + 1)
  })

  output$leafdown <- renderLeaflet({
    rv_update_leafdown()
    update_leafdown_map(my_leafdown, input, df_stations_monthly, my_leafdown$curr_data)
  })

  observeEvent(input$leafdown_glify_click, {
    click_lat <- input$leafdown_glify_click$lat
    click_lng <- input$leafdown_glify_click$lng
    markers <- avg_temp_per_displ_marker(df_stations_monthly, my_leafdown$curr_data)
    new_id <- which(markers$latitude == click_lat & markers$longitude == click_lng)
    if (new_id %in% active_marker_ids) {
      active_marker_ids <<- active_marker_ids[!active_marker_ids == new_id]
    } else {
      active_marker_ids <<- c(active_marker_ids, new_id)
    }
    rv_active_markers(markers[active_marker_ids, ])
    update_markers(active_marker_ids, markers)
  }, ignoreInit = TRUE)

  # plots
  output$line_plot <- renderPlot({
    curr_sel_data <- my_leafdown$curr_sel_data()
    curr_map_level <- my_leafdown$curr_map_level
    active_markers <- rv_active_markers()
    create_line_plot(curr_sel_data, curr_map_level, df_stations_monthly, active_markers)
  })

  output$scatter_plot <- renderPlot({
    curr_sel_data <- my_leafdown$curr_sel_data()
    curr_map_level <- my_leafdown$curr_map_level
    active_markers <- rv_active_markers()
    create_scatter_plot(curr_sel_data, curr_map_level, df_stations_monthly, active_markers)
  })

}
