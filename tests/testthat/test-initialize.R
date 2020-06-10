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
  my_leafdown <- Leafdown$new(spdfs, map_id, input)

  expect_equal(my_leafdown$map_output_id, map_id)
})

test_that("Initialize with wrong 'input'", {
  spdfs <- readRDS("testapps/us1-0005.RDS")

  input <- list(foo = "bar")
  map_id <- "leafdown"
  expect_error(Leafdown$new(spdfs, map_id, input), "The given 'input' argument must be the 'input' from the shiny app")
})

test_that("Initialize with wrong 'map_id'", {
  spdfs <- readRDS("testapps/us1-0005.RDS")

  input <- reactiveValues(foo = "bar")
  map_id <- 12
  expect_error(Leafdown$new(spdfs, map_id, input), "")
})

test_that("Initialize with wrong 'spdfs'", {
  spdfs <- readRDS("testapps/us1-0005.RDS")

  input <- list(foo = "bar")
  map_id <- "leafdown"
  expect_error(Leafdown$new(spdfs, map_id, input), "")
})