library(shinytest)
library(leafdown)
library(shiny)

context("Test 'initialize'")

test_that("Correct initialize works", {
  l1 <- readRDS("testapps/us1-0005.RDS")
  l2 <- readRDS("testapps/us2-0005.RDS")
  spdfs <- list(l1, l2)

  input <- reactiveValues(foo = "bar")
  map_id <- "leafdown"
  expect_error(Leafdown$new(spdfs, map_id, input), NA)
})

test_that("Initialize with wrong 'input'", {
  l1 <- readRDS("testapps/us1-0005.RDS")
  spdfs <- list(l1)

  input <- list(foo = "bar")
  map_id <- "leafdown"
  expect_error(Leafdown$new(spdfs, map_id, input), "The given 'input' argument must be the 'input' from the shiny app")
})

test_that("Initialize with wrong 'map_id'", {
  l1 <- readRDS("testapps/us1-0005.RDS")
  spdfs <- list(l1)

  input <- reactiveValues(foo = "bar")
  map_id <- 12
  expect_error(Leafdown$new(spdfs, map_id, input), "")
})

test_that("Initialize with wrong 'spdf_list'", {
  spdfs <- list("foo")

  input <- reactiveValues(foo = "bar")
  map_id <- "leafdown"
  expect_error(Leafdown$new(spdfs, map_id, input), "The given spdfs_list must contain s4 classes of type SpatialPolygonsDataFrame")
})

test_that("Initialize with wrong 'spdfs'", {
  l1 <- readRDS("testapps/us1-0005.RDS")
  spdfs <- list(l1)

  input <- list(foo = "bar")
  map_id <- "leafdown"
  expect_error(Leafdown$new(spdfs, map_id, input), "")
})

test_that("Correct initialize with different join_map_levels_by works", {
  l1 <- readRDS("testapps/us1-0005.RDS")
  names(l1)[names(l1) == "GID_1"] <- "ISO2"
  l2 <- readRDS("testapps/us2-0005.RDS")
  spdfs <- list(l1, l2)

  input <- reactiveValues(foo = "bar")
  map_id <- "leafdown"
  expect_error(Leafdown$new(spdfs, map_id, input, join_map_levels_by = c("ISO2" = "GID_1")), NA)
})

test_that("Initialize with flipped join_map_levels_by works", {
  l1 <- readRDS("testapps/us1-0005.RDS")
  names(l1)[names(l1) == "GID_1"] <- "ISO2"
  l2 <- readRDS("testapps/us2-0005.RDS")
  spdfs <- list(l1, l2)

  input <- reactiveValues(foo = "bar")
  map_id <- "leafdown"
  expect_error(Leafdown$new(spdfs, map_id, input, join_map_levels_by = c("GID_1" = "ISO2")), NA)
})

test_that("Initialize with wrong join_map_levels_by fails correctly", {
  l1 <- readRDS("testapps/us1-0005.RDS")
  l2 <- readRDS("testapps/us2-0005.RDS")
  spdfs <- list(l1, l2)

  input <- reactiveValues(foo = "bar")
  map_id <- "leafdown"
  expect_error(Leafdown$new(spdfs, map_id, input, join_map_levels_by = c("ISO" = "GID_1")),
               "The given join_map_levels_by must specify the columns to join the map levels by.")
})

