#' Leafdown R6 Class
#'
#' @importFrom magrittr "%>%"
#' @export
Leafdown <- R6::R6Class("Leafdown",
  private = list(
    .spdfs_list = NULL,
    .curr_data = NULL,
    .curr_map_level = NULL,
    .curr_proxy = NULL,
    .curr_selection = NULL,
    .map_output_id = NULL,
    .curr_spdf = NULL,
    .selected_parents = NULL,
    .unselected_parents = NULL,
    .all_poly_ids = NULL,
    add_click_observer = function(input, map_output_id) {
      observeEvent(input[[paste0(map_output_id, "_shape_click")]], {
        clicked_id <- input[[paste0(map_output_id, "_shape_click")]]$id
        req(clicked_id)
        curr_selection <- private$.curr_selection[[private$.curr_map_level]]
        if (clicked_id %in% curr_selection) {
          private$.curr_proxy %>% hideGroup(clicked_id)
          private$.curr_selection[[private$.curr_map_level]] <- curr_selection[!curr_selection == clicked_id]
        } else {
          private$.curr_proxy %>% showGroup(clicked_id)
          private$.curr_selection[[private$.curr_map_level]] <- c(curr_selection, clicked_id)
        }
      })
    }
  ),
  active = list(
    spdfs_list = function(value) {
      if (missing(value)) {
        private$.spdfs_list
      } else {
        stop("`$spdfs_list` is read only", call. = FALSE)
      }
    },
    curr_selection = function(value) {
      if (missing(value)) {
        private$.curr_selection[[private$.curr_map_level]]
      } else {
        stop("`$.curr_selection` is read only", call. = FALSE)
      }
    },
    map_output_id = function(value) {
      if (missing(value)) {
        private$.map_output_id
      } else {
        stop("`$map_output_id` is read only", call. = FALSE)
      }
    },
    curr_map_level = function(value) {
      if (missing(value)) {
        private$.curr_map_level
      } else {
        stop("`$curr_map_level` is read only", call. = FALSE)
      }
    },
    curr_spdf = function(value) {
      if (missing(value)) {
        private$.curr_spdf
      } else {
        stop("`$curr_spdf` is read only", call. = FALSE)
      }
    }
  ),
  public = list(
    initialize = function(spdfs_list, map_output_id, input) {
      private$.spdfs_list <- spdfs_list
      private$.curr_map_level <- 1
      private$.curr_selection <- list(c())
      private$.map_output_id <- map_output_id
      private$.curr_spdf <- private$.spdfs_list[[private$.curr_map_level]]
      private$add_click_observer(input, map_output_id)
      private$.selected_parents <- c()
    },
    draw_leafdown = function(...) {
      curr_spdf <- private$.curr_spdf
      curr_spdf@data <- private$.curr_data
      private$.curr_proxy <- leaflet::leafletProxy(private$.map_output_id)
      all_poly_ids <- c()
      for (pol in curr_spdf@polygons) {
        all_poly_ids <- c(all_poly_ids, pol@ID)
      }
      private$.all_poly_ids <- all_poly_ids
      map <- leaflet::leaflet(curr_spdf) %>%
        leaflet::addPolygons(layerId = ~all_poly_ids, ...) %>%
        addPolylines(
          group = all_poly_ids, stroke = TRUE, weight = 4, color = "#FFCC00",
          highlight = highlightOptions(bringToFront = T, weight = 4)
        )
      if (private$.curr_map_level != 1) {
        # If there are inactive parent polygons then draw them as grey background
        if (length(private$.unselected_parents@polygons) > 0) {
          map <- map %>%
            addPolylines(
              data = private$.unselected_parents,
              stroke = F, weight = 2, color="#929292",
              highlight = highlightOptions(bringToFront = T)
            ) %>%
            addPolygons(
              data = private$.unselected_parents,
              fillOpacity = 0.4,
              color = "#A4A4A5",
              weight = 1
            )
        }
      }
      private$.curr_proxy %>%
        hideGroup(all_poly_ids) %>%
        showGroup(private$.curr_selection[[private$.curr_map_level]])
      map
    },
    get_current_data = function() {
      private$.curr_spdf@data
    },
    add_data = function(data) {
      # TODO: check if all cols of private$.curr_spdf@data are matched in the given @data
      # -> the given @data should contain all columns from private$.curr_spdf@data (plus y)
      # data[, names(private$.curr_spdf@data)] == private$.curr_spdf@data
      private$.curr_data <- data
    },
    drill_down = function() {
      # TODO: check whether we can drill_down further

      # Information about parent polygons
      parents <- private$.spdfs_list[[private$.curr_map_level]]
      all_poly_ids_parents <- private$.all_poly_ids
      curr_selection_parents <- private$.curr_selection[[private$.curr_map_level]]
      index_sel_parents <- all_poly_ids_parents %in% curr_selection_parents
      private$.selected_parents <- parents[index_sel_parents, ]
      private$.unselected_parents <- parents[!index_sel_parents, ]

      # spdf_new contains the child polygons of the selected parents
      spdf_new <- private$.spdfs_list[[private$.curr_map_level + 1]]
      spdf_new <- spdf_new[spdf_new@data$GID_1 %in% private$.selected_parents@data$GID_1, ]

      # Update leafdown object
      private$.curr_spdf <- spdf_new
      private$.curr_map_level <- private$.curr_map_level + 1
      private$.curr_selection[[private$.curr_map_level]] <- character(0)
    },
    drill_up = function() {
      # TODO: check whether we can drill_up further
      private$.curr_spdf <- private$.spdfs_list[[private$.curr_map_level - 1]]
      private$.curr_map_level <- private$.curr_map_level - 1
      private$.unselected_parents <- NULL
    }
  )
)
