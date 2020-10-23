#' Checks whether an object is an s4 class of type SpatialPolygonsDataFrame
#'
#' @param x Object to check.
#' @return TRUE if `x` is an s4 class of type SpatialPolygonsDataFrame
#'
#' @noRd
check_s4_spdf = function (x) {
  isS4(x) && checkmate::test_class(x, "SpatialPolygonsDataFrame")
}


#' Checks for undesired arguments in ellipsis in $draw_leafdown method
#'
#' @param ... Additional arguments given to \code{leaflet::addPolygons}
#'
#' @description
#' Checks arguments in ellipsis for undesired inputs such as `layerId` which may
#' collide with internal structure of leafdown and returns a "cleaned" version of
#' the arguments by removing or redefining problematic inputs.
#' e.g. `layerId` is removed from arg_list when set.
#'
#' @return List containing arguments in ... as elements
check_draw_ellipsis <- function(...) {
  arg_list <- list(...)
  if ("layerId" %in% names(arg_list)) {
    warning("The argument 'layerId' is used internally by leafdown and is therefore ignored")
    arg_list[["layerId"]] <- NULL
  }
  if ("highlight" %in% names(arg_list)) {
    highlight_args <- arg_list$highlight
    if ("bringToFront" %in% names(highlight_args)) {
      warning("The argument 'bringToFront' in 'highlightOptions' is used internally by leafdown and is therefore ignored.")
      arg_list[["highlight"]][["bringToFront"]] <- NULL
    }
    if ("dashArray" %in% names(highlight_args)) {
      warning("The argument 'dashArray' in 'highlightOptions' is used internally by leafdown and is therefore ignored.")
      arg_list[["highlight"]][["dashArray"]] <- NULL
    }
  }
  arg_list
}

#' Check whether the given spdf_list is a valid spdf_list and has all the required params.
#' @description
#' The spdf_list must be a list of at most two elements.
#' All elements must be a s4 class of type SpatialPolygonsDataFrame.
#'
#' @param spdfs_list A list with the spdfs of all map levels
#'
#' @return TRUE if spdf_list is valid.
check_spdf_list <- function (spdfs_list) {
  if(!is.list(spdfs_list)) {
    stop("The given spdfs_list must be a list")
  }
  if(length(spdfs_list) > 2) {
    stop("Leafdown currently supports only two map levels. The given spdf_list can therefore only contain two elements.")
  }
  for(i in length(spdfs_list)) {
    # Check whether the given spdf_element is an s4 class of type SpatialPolygonsDataFrame.
    is_valid <- check_s4_spdf(spdfs_list[[i]])
    if(!is_valid) {
      stop("The given spdfs_list must contain s4 classes of type SpatialPolygonsDataFrame")
    }
  }
}

#' Check whether the given join_map_levels_by is valid
#'
#' @description
#' The join_map_levels_by must be a named vector of at most one element.
#' The columns specified in the vector must be data slots of the spdfs in the spdfs_list.
#'
#' @param join_map_levels_by A named vector with the columns to join the map levels by.
#' @param spdfs_list A list with the spdfs of all map levels.
#' @return the join_map_levels_by in the right order
check_join_map_levels_by <- function (join_map_levels_by, spdfs_list) {
  if (!is.vector(join_map_levels_by)) {
    stop("The given join_map_levels_by must be a vector")
  }
  if (is.null(names(join_map_levels_by))) {
    stop("The given join_map_levels_by must be a NAMED vector.")
  }
  if (length(join_map_levels_by) > 1) {
    stop("Leafdown currently supports only two map levels. The given join_map_levels_by can therefore only contain one named element.")
  }

  if (names(join_map_levels_by[1]) %in% names(spdfs_list[[1]]) & join_map_levels_by[1] %in% names(spdfs_list[[2]])) {
    join_by <- join_map_levels_by
  } else if (names(join_map_levels_by[1]) %in% names(spdfs_list[[2]]) & join_map_levels_by[1] %in% names(spdfs_list[[1]])) {
    join_by <- c(names(join_map_levels_by[1]))
    names(join_by) <- join_map_levels_by[1]
  } else {
    stop("The given join_map_levels_by must specify the columns to join the map levels by.")
  }
  join_by
}
