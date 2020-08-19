#' Checks whether an object is an s4 class of type SpatialPolygonsDataFrame
#'
#' @param x Object to check.
#' @return TRUE if `x` is an s4 class of type SpatialPolygonsDataFrame
#'
#' @noRd
check_s4_spdf = function (x) {
  isS4(x) && checkmate::test_class(x, "SpatialPolygonsDataFrame")
}
