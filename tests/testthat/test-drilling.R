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
  curr_selection <- my_leafdown$.__enclos_env__$private$.curr_selection
  expect_equal(curr_selection[[1]], c("6", "7"))


  # drill down
  app$setInputs(drill_down = "click")
  curr_map_level <- app$getAllValues()$export$my_leafdown$curr_map_level
  my_leafdown <- app$getAllValues()$export$my_leafdown
  expect_equal(my_leafdown$curr_map_level, 2)
  curr_selection <- my_leafdown$.__enclos_env__$private$.curr_selection
  expect_equal(length(curr_selection), 2)
  expect_equal(length(curr_selection[[2]]), 0)

  # check that the drilldown drilled to the correct child shapes
  children <- app$getAllValues()$export$my_leafdown$curr_spdf$GID_1 # e.g USA.6_1
  child_ids <- substr(children, 5, 5)
  expect_true(all(child_ids %in% c("6", "7")))

  app$stop()
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


test_that("correctly selected and unselected parents after drill_down", {
  app <- ShinyDriver$new("testapps")

  # select shapes with ids="6", "7"
  selected_shape <- list(id = "6")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  selected_shape <- list(id = "7")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown

  # drill down
  app$setInputs(drill_down = "click")
  curr_map_level <- app$getAllValues()$export$my_leafdown$curr_map_level
  my_leafdown <- app$getAllValues()$export$my_leafdown
  expect_equal(my_leafdown$curr_map_level, 2)
  expect_equal(length(my_leafdown$curr_selection), 0)

  all_parent_ids <- app$getAllValues()$export$my_leafdown$spdfs_list[[1]]$GID_1
  sel_parents <- app$getAllValues()$export$my_leafdown$.__enclos_env__$private$.selected_parents$GID_1 # e.g USA.6_1
  sel_parents_ids <- substr(sel_parents, 5, 5)
  unsel_parents <- app$getAllValues()$export$my_leafdown$.__enclos_env__$private$.unselected_parents$GID_1 # e.g USA.6_1
  unsel_parents_ids <- substr(sel_parents, 5, 5)

  # check that the selected parents are correct after drill_down
  expect_true(all(sel_parents_ids %in% c("6", "7")))
  expect_true(all(!sel_parents %in% unsel_parents))

  # check that the unselected parents are correct after drill_down
  expect_true(all(!unsel_parents %in% c("6", "7")))
  expect_true(all(!unsel_parents %in% sel_parents))

  app$stop()

})


