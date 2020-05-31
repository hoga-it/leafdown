#' Leafdown R6 Class
#'
#' @importFrom magrittr "%>%"
#' @export
Leafdown <- R6::R6Class("Leafdown",
  private = list(
    .spdfs_list = NULL,
    .curr_data = NULL,
    .curr_map_level = NULL
  ),
  active = list(
    spdfs_list = function(value) {
      if (missing(value)) {
        private$.spdfs_list
      } else {
        stop("`$spdfs_list` is read only", call. = FALSE)
      }
    }
  ),
  public = list(
    initialize = function(spdfs_list) {
      private$.spdfs_list <- spdfs_list
      private$.curr_map_level <- 1
    },
    draw_leafdown = function(...) {
      curr_spdf <- private$.spdfs_list[[private$.curr_map_level]]
      curr_spdf@data <- private$.curr_data
      leaflet::leaflet(curr_spdf) %>%
        leaflet::addPolygons(...)
    },
    get_current_data = function () {
      private$.spdfs_list[[private$.curr_map_level]]@data
    },
    add_data = function (data) {
      private$.curr_data <- data
    }
  )
)
