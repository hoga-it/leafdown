library(shinytest)
library(leafdown)

context("SmokeTests")

test_that("SmokeTest1: Select -> drill down -> drill up should work", {
  app <- ShinyDriver$new("testapps")

  # select shapes with ids="6", "32"
  selected_shape <- list(id = "6")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  selected_shape <- list(id = "32")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown
  expect_equal(my_leafdown$curr_map_level, 1)
  expect_equal(length(my_leafdown$curr_selection), 2)
  expect_true("6" %in% my_leafdown$curr_selection)
  expect_true("32" %in% my_leafdown$curr_selection)

  # unselect shape with id="32"
  selected_shape <- list(id = "6")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown
  expect_equal(my_leafdown$curr_map_level, 1)
  expect_equal(length(my_leafdown$curr_selection), 1)
  print(my_leafdown$curr_selection)
  expect_true("32" %in% my_leafdown$curr_selection)

  # drill down
  app$setInputs(drill_down = "click")
  curr_map_level <- app$getAllValues()$export$my_leafdown$curr_map_level
  my_leafdown <- app$getAllValues()$export$my_leafdown
  expect_equal(my_leafdown$curr_map_level, 2)
  expect_equal(length(my_leafdown$curr_selection), 0)

  # select shapes with ids="281", "268"
  selected_shape <- list(id = "281")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  selected_shape <- list(id = "268")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown
  expect_equal(my_leafdown$curr_selection, c("281", "268"))

  #drill up
  app$setInputs(drill_up = "click")
  curr_map_level <- app$getAllValues()$export$my_leafdown$curr_map_level
  my_leafdown <- app$getAllValues()$export$my_leafdown
  expect_equal(my_leafdown$curr_map_level, 1)
  expect_equal(my_leafdown$curr_selection, "32")
})
