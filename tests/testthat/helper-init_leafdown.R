init_leafdown <- function () {
  states <- readRDS("testapps/us1-0005.RDS")
  states2 <- readRDS("testapps/us2-0005.RDS")
  spdfs_list <- list(states, states2)
  input <- reactiveValues(foo = "bar")
  my_leafdown <- Leafdown$new(spdfs_list, "leafdown", input)
  my_leafdown
}
