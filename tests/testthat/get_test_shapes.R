library(raster)
library(rmapshaper)

# Germany
ger0 <- raster::getData(country = "Germany", level = 0)
ger0 <- rmapshaper::ms_simplify(ger0, keep = 0.005)
saveRDS(ger0, "tests/testthat/res/ger0-0005.RDS")

ger1 <- raster::getData(country = "Germany", level = 1)
ger1 <- rmapshaper::ms_simplify(ger1, keep = 0.005)
saveRDS(ger1, "tests/testthat/res/ger1-0005.RDS")

ger2 <- raster::getData(country = "Germany", level = 2)
ger2 <- rmapshaper::ms_simplify(ger2, keep = 0.005)
saveRDS(ger2, "tests/testthat/res/ger2-0005.RDS")

# US
usa0 <- raster::getData(country = "USA", level = 0)
usa0 <- rmapshaper::ms_simplify(usa0, keep = 0.1)
saveRDS(usa0, "tests/testthat/res/usa0-0005.RDS")

usa1 <- raster::getData(country = "USA", level = 1)
usa1 <- rmapshaper::ms_simplify(usa1, keep = 0.005)
saveRDS(usa1, "tests/testthat/res/usa1-0005.RDS")

usa2 <- raster::getData(country = "USA", level = 2)
usa2 <- rmapshaper::ms_simplify(usa2, keep = 0.005)
saveRDS(usa2, "itests/testthat/res/usa2-0005.RDS")
