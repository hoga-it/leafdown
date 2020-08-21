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
  setClass("student", slots=list(name="character", age="numeric", GPA="numeric"))
  not_spdf <- new("student",name="John", age=21, GPA=3.5)
  expect_false(check_s4_spdf(not_spdf))
})
