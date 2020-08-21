#' Checks whether an object is an s4 class of type SpatialPolygonsDataFrame
#'
#' @param x Object to check.
#' @return TRUE if `x` is an s4 class of type SpatialPolygonsDataFrame
#'
#' @noRd
check_s4_spdf = function (x) {
  isS4(x) && checkmate::test_class(x, "SpatialPolygonsDataFrame")
}



check_draw_ellipsis <- function(...) {
  arg_list <- list(...)
  if ("layerId" %in% names(arg_list)) {
    warning("The argument 'layerId' is used internally by leafdown and is therefore ignored")
    arg_list[["layerId"]] <- NULL
  }
  arg_list
}


#check_draw_ellipsis(layerId = 1, a = 2)
