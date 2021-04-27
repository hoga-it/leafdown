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
#' \url{https://www.deutschlandinzahlen.de}
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


#' Results of the 2016 US Presidential Election - County Level
#'
#'
#' A dataset containing the results of the presidential election and census data (e.g. racial makeup, unemployment)
#'
#' @format A data frame with 3,143 rows and 17 total columns
#' \describe{
#'   \item{State}{Name of the State}
#'   \item{ST}{Abbreviation of the State name}
#'   \item{County}{Name of the County}
#'   \item{Votes}{Total number of votes cast}
#'   \item{Republicans2016}{Percent of votes for the Republican Party}
#'   \item{Democrats2016}{Percent of votes for the Democratic Party}
#'   \item{Green2016}{Percent of votes for the Green Party}
#'   \item{Libertarians2016}{Percent of votes for the Libertarian Party}
#'   \item{TotalPopulation}{Total Population of the county}
#'   \item{Unemployment}{Percent of unemployment}
#'   \item{White}{Percentage of Whites}
#'   \item{Black}{Percentage of Blacks}
#'   \item{Hispanic}{Percentage of Hispanics}
#'   \item{Asian}{Percentage of Asians}
#'   \item{Amerindian}{Percentage of Amerindians}
#'   \item{Other}{Percentage of Other Races}
#'   \item{NAME_2}{The short County name, used for matching with the map}
#' }
#' @source
#' https://github.com/Deleetdk/USA.county.data
"us_election_counties"

#' Results of the 2016 US Presidential Election - State Level
#'
#'
#' A dataset containing the results of the presidential election and census data (e.g. racial makeup, unemployment)
#'
#' @format A data frame with 51 rows and 15 total columns
#' \describe{
#'   \item{State}{Name of the State}
#'   \item{ST}{Abbreviation of the State name}
#'   \item{Votes}{Total number of votes cast}
#'   \item{Republicans2016}{Percent of votes for the Republican Party}
#'   \item{Democrats2016}{Percent of votes for the Democratic Party}
#'   \item{Green2016}{Percent of votes for the Green Party}
#'   \item{Libertarians2016}{Percent of votes for the Libertarian Party}
#'   \item{TotalPopulation}{Total Population of the county}
#'   \item{Unemployment}{Percent of unemployment}
#'   \item{White}{Percentage of Whites}
#'   \item{Black}{Percentage of Blacks}
#'   \item{Hispanic}{Percentage of Hispanics}
#'   \item{Asian}{Percentage of Asians}
#'   \item{Amerindian}{Percentage of Amerindians}
#'   \item{Other}{Percentage of Other Races}
#' }
#' @source
#' https://github.com/Deleetdk/USA.county.data
#'
#' Note: The data was aggregated from the county level
"us_election_states"
