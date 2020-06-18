library(shinytest)
library(leafdown)
library(leaflet)

context("Selection")

test_that("selection and deselection works", {
  app <- ShinyDriver$new("testapps")

  # select shapes with id="6"
  selected_shape <- list(id = "6")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown

  # select shape with id="32"
  selected_shape <- list(id = "32")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown

  # unselect shape with id="6"
  selected_shape <- list(id = "6")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown

  expect_equal(length(my_leafdown$curr_selection), 1)
  expect_true("32" %in% my_leafdown$curr_selection)
})
