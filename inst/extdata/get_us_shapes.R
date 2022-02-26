library(raster)
library(rmapshaper)

usa0 <- raster::getData(country = "USA", level = 0)
usa0 <- rmapshaper::ms_simplify(usa0)
saveRDS(usa0, "inst/extdata/usa0.RDS")

usa1 <- raster::getData(country = "USA", level = 1)
usa1 <- rmapshaper::ms_simplify(usa1, keep = 0.5)
saveRDS(usa1, "inst/extdata/usa1.RDS")

usa2 <- raster::getData(country = "USA", level = 2)
usa2 <- rmapshaper::ms_simplify(usa2, keep = 0.5)
saveRDS(usa2, "inst/extdata/usa2.RDS")
