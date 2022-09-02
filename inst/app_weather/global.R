library(leafdown)
library(leaflet)
library(shiny)
library(dplyr)
library(shinyjs)
library(leafgl)
library(sf)
library(ggplot2)
library(plotly)
library(bs4Dash)
library(fresh)
library(ggrepel)
library(readr)

helper_files = list.files(path = "helper", full.names = TRUE, pattern = "*.R")
sapply(helper_files, source, encoding = "UTF-8")

# shapes and data -----------------------------------------------------------------
usa1 <- readRDS("../../inst/extdata/usa1.RDS")
usa2 <- readRDS("../../inst/extdata/usa2.RDS")
spdfs_list <- list(usa1, usa2, usa2)
df_stations_monthly <- get_data()




