library(shinytest)
library(leafdown)

context("Drilling_Multi_Level")

# Due to Error: PhantomJS not found.
skip_on_cran()
skip_on_ci()

test_that("drill_down drills multiple times correctly", {
  app <- ShinyDriver$new("testapp_multi_level")

  # select shapes with ids="1", "2"
  selected_shape <- list(id = "1")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  selected_shape <- list(id = "2")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown
  expect_equal(my_leafdown$curr_map_level, 1)
  curr_selection <- my_leafdown$.__enclos_env__$private$.curr_sel_ids
  expect_equal(curr_selection[[1]], c("1", "2"))

  # drill down
  app$setInputs(drill_down = "click")
  curr_map_level <- app$getAllValues()$export$my_leafdown$curr_map_level
  my_leafdown <- app$getAllValues()$export$my_leafdown
  expect_equal(my_leafdown$curr_map_level, 2)
  curr_selection <- my_leafdown$.__enclos_env__$private$.curr_sel_ids
  expect_equal(length(curr_selection), 2)
  expect_equal(length(curr_selection[[2]]), 0)

  # check that the drilldown drilled to the correct child shapes
  children <- app$getAllValues()$export$my_leafdown$curr_spdf$GID_1 # e.g USA.6_1
  child_ids <- substr(children, 5, 5)
  expect_true(all(child_ids %in% c("1", "2")))

  ##
  # drill to next level
  ##

  # select shapes with ids="44", "53"
  selected_shape <- list(id = "44")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  selected_shape <- list(id = "53")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown
  expect_equal(my_leafdown$curr_map_level, 2)
  curr_selection <- my_leafdown$.__enclos_env__$private$.curr_sel_ids
  expect_equal(curr_selection[[1]], c("1", "2"))
  expect_equal(curr_selection[[2]], c("44", "53"))

  # drill down
  app$setInputs(drill_down = "click")
  curr_map_level <- app$getAllValues()$export$my_leafdown$curr_map_level
  my_leafdown <- app$getAllValues()$export$my_leafdown
  expect_equal(my_leafdown$curr_map_level, 3)
  curr_selection <- my_leafdown$.__enclos_env__$private$.curr_sel_ids
  expect_equal(length(curr_selection), 3)

  # check that the drilldown drilled to the correct child shapes
  expect_equal(curr_selection[[1]], c("1", "2"))
  expect_equal(curr_selection[[2]], c("44", "53"))
  expect_equal(length(curr_selection[[3]]), 0)

  app$stop()
})

test_that("cannot drill up higher than level 1 in multi-level maps", {
  app <- ShinyDriver$new("testapp_multi_level")
  app$setInputs(drill_up = "click")

  leafdown_output <- app$getAllValues()$output$leafdown
  expect_false("message" %in% names(leafdown_output))

  app$stop()
})

test_that("cannot drill down lower than lowest level in multi-level maps", {
  app <- ShinyDriver$new("testapp_multi_level")
  selected_shape <- list(id = "1")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  app$setInputs(drill_down = "click")

  selected_shape <- list(id = "44")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  app$setInputs(drill_down = "click")

  my_leafdown <- app$getAllValues()$export$my_leafdown
  expect_equal(my_leafdown$curr_map_level, 3)

  selected_shape <- list(id = "2528")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  app$setInputs(drill_down = "click")

  leafdown_output <- app$getAllValues()$output$leafdown
  expect_false("message" %in% names(leafdown_output))

  app$stop()
})
