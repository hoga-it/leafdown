library(leafdown)
library(shiny)

context("Test 'add_data'")

test_that("Add data correctly", {
  my_leafdown <- init_leafdown()
  data <- my_leafdown$get_current_data()
  data$y <- 1:nrow(data)
  my_leafdown$add_data(data)

  expect_true(identical(data, my_leafdown$curr_data))
})

test_that("Add 'Null' as data throws error", {
  my_leafdown <- init_leafdown()
  expect_error(my_leafdown$add_data(NULL), "The given data must be a list")
})

test_that("Add empty List as data throws error", {
  my_leafdown <- init_leafdown()

  expect_error(my_leafdown$add_data(list()), "You cannot remove columns from the existing data. Only add to it")
})

test_that("Changed values in data throws error", {
  my_leafdown <- init_leafdown()
  data <- my_leafdown$get_current_data()

  # change data
  # random col and row for change
  col <- floor(runif(1, min=1, max=dim(data)[2]))
  row <- floor(runif(1, min=1, max=dim(data)[1]))
  data[row, col] <- NA

  expect_error(my_leafdown$add_data(data), "You cannot change the existing data. Only add to it")
})

test_that("Missing columns in data throws error", {
  my_leafdown <- init_leafdown()
  data <- my_leafdown$get_current_data()

  # change data
  # random col to delete
  col <- floor(runif(1, min=1, max=dim(data)[2]))
  data <- data[, -col]

  expect_error(my_leafdown$add_data(data), "You cannot remove columns from the existing data. Only add to it")
})

test_that("Missing row in data throws error", {
  my_leafdown <- init_leafdown()
  data <- my_leafdown$get_current_data()

  # change data
  # random col and row for change
  row <- floor(runif(1, min=1, max=dim(data)[1]))
  data <- data[-row, ]

  expect_error(my_leafdown$add_data(data), "You cannot change the existing data. Only add to it")
})

