
file.copy(list.files("inst/extdata/", full.names = TRUE), "inst/app_dach/")
rsconnect::deployApp(appDir = "inst/app_dach", appName = "leafdown-multi-level", lint = FALSE,
                     appPrimaryDoc = "app_dach.R", account = "andreasho95")
file.remove("inst/app_dach/ger0-005.RDS")
file.remove("inst/app_dach/ger1-005.RDS")
file.remove("inst/app_dach/ger2-005.RDS")
file.remove("inst/app_dach/a0-005.RDS")
file.remove("inst/app_dach/a1-005.RDS")
file.remove("inst/app_dach/a2-005.RDS")
file.remove("inst/app_dach/ch0-005.RDS")
file.remove("inst/app_dach/ch1-005.RDS")
file.remove("inst/app_dach/ch2-005.RDS")
