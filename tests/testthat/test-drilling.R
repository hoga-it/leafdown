library(shinytest)
library(leafdown)

context("Drilling")

test_that("drill_down drills to correct subshapes", {
  app <- ShinyDriver$new("testapps")

  # select shapes with ids="6", "7"
  selected_shape <- list(id = "6")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  selected_shape <- list(id = "7")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown
  expect_equal(my_leafdown$curr_map_level, 1)
  expect_equal(my_leafdown$curr_selection, c("6", "7"))

  # drill down
  app$setInputs(drill_down = "click")
  curr_map_level <- app$getAllValues()$export$my_leafdown$curr_map_level
  my_leafdown <- app$getAllValues()$export$my_leafdown
  expect_equal(my_leafdown$curr_map_level, 2)
  expect_equal(length(my_leafdown$curr_selection), 0)

  # check that the drilldown drilled to the correct child shapes
  children <- app$getAllValues()$export$my_leafdown$curr_spdf$GID_1 # e.g USA.6_1
  child_ids <- substr(children, 5, 5)
  expect_true(all(child_ids %in% c("6", "7")))
})

test_that("cannot drill up higher than level 1", {
  app <- ShinyDriver$new("testapps")
  app$setInputs(drill_up = "click")

  leafdown_output <- app$getAllValues()$output$leafdown
  expect_false("message" %in% names(leafdown_output))

  app$stop()
})

test_that("cannot drill down lower than lowest level", {
  app <- ShinyDriver$new("testapps")
  app$setInputs(drill_down = "click")

  leafdown_output <- app$getAllValues()$output$leafdown
  expect_false("message" %in% names(leafdown_output))

  app$stop()
})

test_that("cannot drill down without selection", {
  app <- ShinyDriver$new("testapps")

  app$setInputs(drill_down = "click")
  leafdown_output <- app$getAllValues()$output$leafdown
  expect_false("message" %in% names(leafdown_output))

  app$stop()
})
