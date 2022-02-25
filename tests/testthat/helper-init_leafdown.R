init_leafdown <- function () {
  states <- readRDS("../../inst/extdata/usa1.RDS")
  states2 <- readRDS("../../inst/extdata/usa2.RDS")
  spdfs_list <- list(states, states2)
  input <- shiny::reactiveValues(foo = "bar")
  my_leafdown <- Leafdown$new(spdfs_list, "leafdown", input)
  my_leafdown
}
