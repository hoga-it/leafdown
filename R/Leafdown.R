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
    add_click_observer = function (input, map_output_id) {
      observeEvent(input[[paste0(map_output_id, "_shape_click")]], {
        clicked_id <- input[[paste0(map_output_id, "_shape_click")]]$id
        if(clicked_id %in% private$.curr_selection) {
          private$.curr_proxy %>% hideGroup(clicked_id)
          private$.curr_selection <- private$.curr_selection[-which(private$.curr_selection == clicked_id)]
        } else {
          private$.curr_proxy %>% showGroup(clicked_id)
          private$.curr_selection <- c(private$.curr_selection, clicked_id)
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
        private$.curr_selection
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
    }
  ),
  public = list(
    initialize = function(spdfs_list, map_output_id, input) {
      private$.spdfs_list <- spdfs_list
      private$.curr_map_level <- 1
      private$.curr_selection <- c()
      private$.map_output_id <- map_output_id
      private$add_click_observer(input, map_output_id)
    },
    draw_leafdown = function(...) {
      curr_spdf <- private$.spdfs_list[[private$.curr_map_level]]
      curr_spdf@data <- private$.curr_data
      private$.curr_proxy = leaflet::leafletProxy(private$.map_output_id)

      all_ids <- c()
      for(pol in curr_spdf@polygons) {
        all_ids <- c(all_ids, pol@ID)
      }

      map <- leaflet::leaflet(curr_spdf) %>%
        leaflet::addPolygons(layerId = ~all_ids, ...) %>% addPolylines(
          group = all_ids, stroke = TRUE, weight = 4,color = "#FFCC00",
          highlight = highlightOptions(bringToFront = T, weight = 4))

      private$.curr_proxy %>% hideGroup(all_ids)

      map
    },
    get_current_data = function () {
      private$.spdfs_list[[private$.curr_map_level]]@data
    },
    add_data = function (data) {
      private$.curr_data <- data
    }
  )
)





