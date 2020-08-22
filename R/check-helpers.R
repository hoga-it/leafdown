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
#' Checks arguments in ellipsis for undesired inputs such as 'layerId' which may
#' collide with internal structure of leafdown and returns a "cleaned" version of
#' the arguments by removing or redefining problematic inputs.
#' e.g. 'layerId' is removed from arg_list when set.
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
  }
  arg_list
}





