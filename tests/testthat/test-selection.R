library(shinytest)
library(leaflet)

# Due to Error: PhantomJS not found.
skip_on_cran()
skip_on_ci()

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
  curr_sel_ids <- my_leafdown$.__enclos_env__$private$.curr_sel_ids
  expect_equal(length(curr_sel_ids), 1)
  expect_true("32" %in% curr_sel_ids)

  app$stop()
})


test_that("selection and deselection leads to correct curr_sel_data", {
  app <- ShinyDriver$new("testapps")

  # select shapes with id="2" (Alaska)
  selected_shape <- list(id = "2")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown
  expect_true(shiny::is.reactive(my_leafdown$curr_sel_data))
  curr_sel_data <- shiny::isolate({my_leafdown$curr_sel_data()})
  expect_true(is.data.frame(curr_sel_data))
  expect_true(nrow(curr_sel_data) == 1)
  expect_true(curr_sel_data[1, "NAME_1"] == "Alaska")

  # select shape with id="10" (Alaska)
  selected_shape <- list(id = "10")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown
  curr_sel_data <- shiny::isolate({my_leafdown$curr_sel_data()})
  expect_true(is.data.frame(curr_sel_data))
  expect_true(nrow(curr_sel_data) == 2)
  expect_true(curr_sel_data[1, "NAME_1"] == "Alaska")
  expect_true(curr_sel_data[2, "NAME_1"] == "Florida")

  # select shape with id="44" (Texas)
  selected_shape <- list(id = "44")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown
  curr_sel_data <- shiny::isolate({my_leafdown$curr_sel_data()})
  expect_true(is.data.frame(curr_sel_data))
  expect_true(nrow(curr_sel_data) == 3)
  expect_true(curr_sel_data[1, "NAME_1"] == "Alaska")
  expect_true(curr_sel_data[2, "NAME_1"] == "Florida")
  expect_true(curr_sel_data[3, "NAME_1"] == "Texas")

  # unselect shape with id="2" (Alaska)
  selected_shape <- list(id = "2")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown
  curr_sel_data <- shiny::isolate({my_leafdown$curr_sel_data()})
  expect_true(is.data.frame(curr_sel_data))
  expect_true(nrow(curr_sel_data) == 2)
  expect_true(curr_sel_data[1, "NAME_1"] == "Florida")
  expect_true(curr_sel_data[2, "NAME_1"] == "Texas")

  app$stop()
})


test_that("(de-)activating the shape selection works", {
  app <- ShinyDriver$new("testapps")

  # select shape with id = "1"
  selected_shape <- list(id = "1")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown
  curr_sel_ids <- my_leafdown$.__enclos_env__$private$.curr_sel_ids
  expect_true("1" %in% curr_sel_ids)

  # deactivate shape selection
  app$setInputs(deactive_shape_selection = "click")

  # select shape with id = "2"
  selected_shape <- list(id = "2")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown
  curr_sel_ids <- my_leafdown$.__enclos_env__$private$.curr_sel_ids
  expect_false("2" %in% curr_sel_ids)

  # select shape with id = "1"
  selected_shape <- list(id = "1")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown
  curr_sel_ids <- my_leafdown$.__enclos_env__$private$.curr_sel_ids
  expect_true("1" %in% curr_sel_ids)

  # activate shape selection
  app$setInputs(active_shape_selection = "click")

  # select shape with id = "2"
  selected_shape <- list(id = "2")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown
  curr_sel_ids <- my_leafdown$.__enclos_env__$private$.curr_sel_ids
  expect_true("2" %in% curr_sel_ids[[1]])

  # select shape with id = "1"
  selected_shape <- list(id = "1")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  my_leafdown <- app$getAllValues()$export$my_leafdown

  curr_sel_ids <- my_leafdown$.__enclos_env__$private$.curr_sel_ids
  expect_false("1" %in% curr_sel_ids[[1]])

  app$stop()
})


