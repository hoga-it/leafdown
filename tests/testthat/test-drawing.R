library(shinytest)
library(leaflet)

context("Drawing")

test_that("Argument 'layerId' in draw_leafdown is ingored if set", {

  app <- ShinyDriver$new("testapps")

  app$setInputs(args_leaflet = list(layerId = 2), allowInputNoBinding_ = TRUE)

  # select shapes with id="6"
  selected_shape <- list(id = "6")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  warning_msg <- app$getAllValues()$export$eval_draw
  expect_true(grepl("'layerId' is used internally by leafdown and is therefore ignored",
                    warning_msg, fixed = TRUE))

  app$setInputs(args_leaflet = list(layerId = 2), allowInputNoBinding_ = TRUE)

  app$stop()

  # Checks if 'layerId' not set that don't return a warning
  app <- ShinyDriver$new("testapps")

  # select shapes with id="6"
  selected_shape <- list(id = "10")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  warning_msg <- app$getAllValues()$export$eval_draw
  expect_true(is.null(warning_msg))

  app$stop()

})

test_that("highlightOptions argument 'bringToFront' in 'highlightOptions' in draw_leafdown
          is ingored if set", {

  app <- ShinyDriver$new("testapps")
  app$setInputs(args_leaflet = list(highlight = highlightOptions(bringToFront = TRUE)),
                allowInputNoBinding_ = TRUE)

  # select shapes with id="6"
  selected_shape <- list(id = "6")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  warning_msg <- app$getAllValues()$export$eval_draw
  expect_true(grepl("'bringToFront' in 'highlightOptions' is used internally by leafdown and is therefore ignored",
                    warning_msg, fixed = TRUE))

  app$stop()

  # Checks if 'layerId' not set that don't return a warning
  app <- ShinyDriver$new("testapps")

  # select shapes with id="6"
  selected_shape <- list(id = "10")
  app$setInputs(leafdown_shape_click = selected_shape, allowInputNoBinding_ = TRUE)
  warning_msg <- app$getAllValues()$export$eval_draw
  expect_true(is.null(warning_msg))

  app$stop()

})
