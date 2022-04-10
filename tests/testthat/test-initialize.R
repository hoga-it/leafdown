library(shinytest)
library(leafdown)
library(shiny)

context("Test 'initialize'")

test_that("Correct initialize works", {
  l1 <- readRDS("res/usa1.RDS")
  l2 <- readRDS("res/usa2.RDS")
  spdfs <- list(l1, l2)

  input <- reactiveValues(foo = "bar")
  map_id <- "leafdown"
  expect_error(Leafdown$new(spdfs, map_id, input), NA)
})

test_that("Initialize with wrong 'input'", {
  l1 <- readRDS("res/usa1.RDS")
  spdfs <- list(l1)

  input <- list(foo = "bar")
  map_id <- "leafdown"
  expect_error(Leafdown$new(spdfs, map_id, input), "The given 'input' argument must be the 'input' from the shiny app")
})

test_that("Initialize with wrong 'map_id'", {
  l1 <- readRDS("res/usa1.RDS")
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
  l1 <- readRDS("res/usa1.RDS")
  spdfs <- list(l1)

  input <- list(foo = "bar")
  map_id <- "leafdown"
  expect_error(Leafdown$new(spdfs, map_id, input), "")
})

test_that("Correct initialize with different join_map_levels_by works", {
  l1 <- readRDS("res/usa1.RDS")
  names(l1)[names(l1) == "GID_1"] <- "ISO2"
  l2 <- readRDS("res/usa2.RDS")
  spdfs <- list(l1, l2)

  input <- reactiveValues(foo = "bar")
  map_id <- "leafdown"
  expect_error(Leafdown$new(spdfs, map_id, input, join_map_levels_by = c("ISO2" = "GID_1")), NA)
})


test_that("Initialize with wrong join_map_levels_by fails correctly", {
  l1 <- readRDS("res/usa1.RDS")
  l2 <- readRDS("res/usa2.RDS")
  spdfs <- list(l1, l2)

  input <- reactiveValues(foo = "bar")
  map_id <- "leafdown"
  expect_error(Leafdown$new(spdfs, map_id, input, join_map_levels_by = c("ISO" = "GID_1")),
               "columns must be present in spdf data")
})

