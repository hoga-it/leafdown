#' GPD for federal states of Germany for 2014.
#'
#'
#' A dataset containing the GPD (gross domestic product) for all 16 federal states
#' of Germany for the year 2014.
#'
#' @format A data frame with 16 rows and 2 variables:
#' \describe{
#'   \item{Federal_State}{Name of the federal state}
#'   \item{GDP_2014}{GDP for the year 2014, in euro}
#' }
#' @source Arbeitskreis Volkswirtschaftliche Gesamtrechnungen der Laender:
#' \url{http://www.deutschlandinzahlen.de}
"gdp_2014_federal_states"

#' GPD for administrative districts of Germany for 2014.
#'
#'
#' A dataset containing the GPD (gross domestic product) for 402 administrative districts
#' of Germany for the year 2014.
#'
#' @format A data frame with 402 rows and 2 variables:
#' \describe{
#'   \item{Admin_District}{Name of the administrative district}
#'   \item{GDP_2014}{GDP for the year 2014, in euro}
#' }
#' @source
#' Landatlas (www.landatlas.de). Ausgabe 2018.
#' Hrsg.: Thuenen-Institut fuer Laendliche Raeume - Braunschweig 2018.
#'
#' Note that in this package we have slightly adapted some names of the
#' administrative districts for a better match.
"gdp_2014_admin_districts"
