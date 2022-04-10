init_leafdown <- function() {
  states <- readRDS("res/usa1.RDS")
  states2 <- readRDS("res/usa2.RDS")
  spdfs_list <- list(states, states2)
  input <- shiny::reactiveValues(foo = "bar")
  my_leafdown <- Leafdown$new(spdfs_list, "leafdown", input)
  my_leafdown
}
