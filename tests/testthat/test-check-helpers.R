library(shinytest)
library(leaflet)

context("check_s4_spdf")

test_that("function 'check_s4_spdf' correctly detects objects of class s4 and
          type SpatialPolygonsDataFrame", {
  spdf_s4 <- readRDS("testapps/us1-0005.RDS")
  expect_true(check_s4_spdf(spdf_s4))
  expect_false(check_s4_spdf(1:10))
  not_s4 <- 1:10
  class(not_s4) <- "SpatialPolygonsDataFrame"
  expect_false(check_s4_spdf(not_s4))
  setClass("student", slots = list(name = "character", age = "numeric", GPA = "numeric"))
  not_spdf <- new("student", name = "John", age = 21, GPA = 3.5)
  expect_false(check_s4_spdf(not_spdf))
})


context("check_draw_ellipsis")

test_that("function 'check_draw_ellipsis' correctly detects undesired input in $draw_leafdown and returns
          warning message in case", {
  expect_warning(check_draw_ellipsis(layerId = 1, a = 2), "used internally by leafdown")
  expect_warning(check_draw_ellipsis(layerId = 1), "used internally by leafdown")
  expect_warning(
    check_draw_ellipsis(highlight = highlightOptions(bringToFront = TRUE), a = 2),
    "used internally by leafdown"
  )
  expect_warning(
    check_draw_ellipsis(highlight = highlightOptions(bringToFront = TRUE)),
    "used internally by leafdown"
  )
})

empty_named_list <- list()
names(empty_named_list) <- character(0)
test_that("function 'check_draw_ellipsis' removes or changes undesired input in $draw_leafdown correctly", {
  # Tests for 'layerId' argument
  expect_warning(arg_list_clean <- check_draw_ellipsis(layerId = 1, a = 2))
  expect_identical(arg_list_clean, list(a = 2))
  expect_warning(arg_list_clean <- check_draw_ellipsis(layerId = 1))
  expect_true(length(arg_list_clean) == 0)

  # Tests for 'bringToFront' argument
  expect_warning(
    arg_list_clean <- check_draw_ellipsis(
      highlight = highlightOptions(bringToFront = TRUE),
      a = 2
    )
  )
  expect_identical(arg_list_clean, list(highlight = empty_named_list, a = 2))
  expect_warning(
    arg_list_clean <- check_draw_ellipsis(
      highlight = highlightOptions(bringToFront = TRUE)
    )
  )
  expect_identical(arg_list_clean, list(highlight = empty_named_list))
  expect_warning(
    arg_list_clean <- check_draw_ellipsis(
      highlight = highlightOptions(bringToFront = TRUE, color = "blue")
    )
  )
  expect_identical(arg_list_clean, list(highlight = list(color = "blue")))

  # Tests for 'layerId' and 'bringToFront' arguments together
  expect_warning(
    arg_list_clean <- check_draw_ellipsis(
      a = 1,
      highlight = highlightOptions(bringToFront = TRUE),
      layerId = 2
    )
  )
  expect_identical(arg_list_clean, list(a = 1, highlight = empty_named_list))
  expect_warning(
    arg_list_clean <- check_draw_ellipsis(
      highlight = highlightOptions(bringToFront = TRUE), layerId = 1
    )
  )
  expect_identical(arg_list_clean, list(highlight = empty_named_list))
})


test_that("function 'check_draw_ellipsis' deals correctly with proper input in $draw_leafdown", {
  input <- list(a = 1, b = 2)
  # This tests that we !don't! receive a warning message if input is correct
  expect_warning(check_draw_ellipsis(input), regexp = NA)
  expect_identical(list(input), check_draw_ellipsis(input))
  f_with_ellipsis <- function(...) {
    arg_list <- check_draw_ellipsis(...)
    arg_list[[1]]
  }
  expect_identical(input, f_with_ellipsis(input))
})
