library(raster)
library(rmapshaper)

# Germany
ger0 <- raster::getData(country = "Germany", level = 0)
ger0 <- rmapshaper::ms_simplify(ger0)
saveRDS(ger0, "inst/extdata/ger0-005.RDS")

ger1 <- raster::getData(country = "Germany", level = 1)
ger1 <- rmapshaper::ms_simplify(ger1, keep = 0.05)
saveRDS(ger1, "inst/extdata/ger1-005.RDS")

ger2 <- raster::getData(country = "Germany", level = 2)
ger2 <- rmapshaper::ms_simplify(ger2, keep = 0.05)
saveRDS(ger2, "inst/extdata/ger2-005.RDS")

# Austria
a0 <- raster::getData(country = "Austria", level = 0)
a0 <- rmapshaper::ms_simplify(a0)
saveRDS(a0, "inst/extdata/a0-005.RDS")

a1 <- raster::getData(country = "Austria", level = 1)
a1 <- rmapshaper::ms_simplify(a1, keep = 0.05)
saveRDS(a1, "inst/extdata/a1-005.RDS")

a2 <- raster::getData(country = "Austria", level = 2)
a2 <- rmapshaper::ms_simplify(a2, keep = 0.05)
saveRDS(a2, "inst/extdata/a2-005.RDS")

# Switzerland
ch0 <- raster::getData(country = "Switzerland", level = 0)
ch0 <- rmapshaper::ms_simplify(ch0)
saveRDS(ch0, "inst/extdata/ch0-005.RDS")

ch1 <- raster::getData(country = "Switzerland", level = 1)
ch1 <- rmapshaper::ms_simplify(ch1, keep = 0.05)
saveRDS(ch1, "inst/extdata/ch1-005.RDS")

ch2 <- raster::getData(country = "Switzerland", level = 2)
ch2 <- rmapshaper::ms_simplify(ch2, keep = 0.05)
saveRDS(ch2, "inst/extdata/ch2-005.RDS")
