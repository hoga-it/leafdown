
<!-- README.md is generated from README.Rmd. Please edit that file -->

# leafdown <a><img src='man/figures/hex-leafdown.png' align="right" height="139" /></a>

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Travis build
status](https://travis-ci.com/hoga-it/leafdown.svg?branch=master)](https://travis-ci.com/hoga-it/leafdown)
<!-- badges: end -->

The leafdown package provides drilldown functionality for leaflet
choropleths in R Shiny apps.

<img src='man/figures/select_drilldown.PNG'/>

## Installation

You can install the released version of leafdown from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("leafdown")
```

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hoga-it/leafdown")
```

## Documentation

You can find the documentation
[here](https://hoga-it.github.io/leafdown/index.html).

## Features

-   Adds drilldown functionality for [leaflet
    choropleths](https://rstudio.github.io/leaflet/choropleths.html)
    maps.
-   Allows the selection of regions/shapes.
-   Easy to use and well-integrated into syntax of the
    [leaflet](https://rstudio.github.io/leaflet/) R package.
-   Allows communication of the map with other shiny elements
    (e.g. graphs).
-   Computationally efficient as drilldown is only executed for selected
    regions of interest.

## Showcase - Election Map

To showcase the features of the `leafdown` package we have created a
demo app. <br>

This app shows the 2016 us presidential election results as well as some
demographic information.<br> Click here for the [full demo
app](https://pega.shinyapps.io/election16/) and here for the
[documentation](https://hoga-it.github.io/leafdown/articles/Showcase_electionapp.html).
<br> <br>

<img src='man/figures/app_election_map.png'/>
