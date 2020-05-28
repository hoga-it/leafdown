#' Leafdown R6 Class
#'
#' @importFrom magrittr "%>%"
Leafdown <- R6::R6Class("Leafdown",
  private = list(
    .spdfs_list = NULL
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
    },
    create_leafdown = function(map_level = 1, ...) {
      curr_spdf <- private$.spdfs_list[[map_level]]
      additional_args <- list(...)
      leaflet::leaflet(curr_spdf) %>%
        leaflet::addPolygons(...)
    }
  )
)



###
# states <- readRDS("inst/extdata/states.RDS")
# states2 <- readRDS("inst/extdata/states2.RDS")
# spdfs_list <- list(
#   states,
#   states2
# )
#
# my_leafdown <- Leafdown$new(spdfs_list)
# my_leafdown$create_leafdown(
#   map_level = 1,
#   weight = 2,
#   opacity = 1,
#   color = "green",
#   dashArray = "3",
#   fillOpacity = 0.7
# )
# my_leafdown$create_leafdown(
#   map_level = 2,
#   weight = 2,
#   opacity = 1,
#   color = "green",
#   dashArray = "3",
#   fillOpacity = 0.7
# )
