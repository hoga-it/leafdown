
<!-- README.md is generated from README.Rmd. Please edit that file -->

# leafdown

<!-- badges: start -->

<!-- badges: end -->

The goal of leafdown is to provide drill down functionality for leaflet
choropleaths.

## Installation

You can install the released version of leafdown from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("leafdown")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hoga-it/leafdown")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(leafdown)
library(raster)   
#> Loading required package: sp
library(shiny)
library(leaflet)
```

Get spdfs

``` r
ger1 <- getData("GADM", country = "Germany", level = 1)
ger2 <- getData("GADM", country = "Germany", level = 2)
spdfs_list <- list(ger1, ger2)
```
