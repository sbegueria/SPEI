# Build the `tarazona` dataset (data/tarazona.rda).
#
# The raw files are not in the repository. Set the environment variable
# SPEI_TARAZONA_RAW (or edit `raw_dir` below) to the folder that holds them:
#   bai/bai_tarazona.csv          basal area increment (cm2 per year) of all
#                                 plots and cores at the site
#   clima/pr_peninsula_pred.csv   precipitation, mm per period
#   clima/eto_peninsula.csv       reference evapotranspiration, mm per period
#   clima/tmin_peninsula_pred.csv mean daily minimum temperature, degC
#   clima/tmax_peninsula_pred.csv mean daily maximum temperature, degC
# The climate files have one row per period (48 per year, 1961-2020) and one
# column per site. Only the `Tarazona` column is kept.
#
# Run from the package root: source("data-raw/tarazona.R")

raw_dir <- Sys.getenv(
  "SPEI_TARAZONA_RAW",
  "~/Downloads/angela_red_dendro/data_raw"
)

# Basal area increment ----------------------------------------------------

# The file has a 9-line header: site, elevation, lat;lon, species, thinning
# year, study period, plot codes, treatments and core letters.
bai_file <- file.path(raw_dir, "bai", "bai_tarazona.csv")
meta <- readLines(bai_file, n = 9, encoding = "UTF-8")
meta <- sub("^﻿", "", meta)
latlon <- as.numeric(strsplit(meta[3], ";")[[1]])

bai <- read.table(bai_file,
  sep = ";", skip = 9, header = TRUE,
  fileEncoding = "UTF-8-BOM"
)
# Keep the control (unthinned) plot, TAPHD
bai <- bai[, c("year", grep("^TAPHD", names(bai), value = TRUE))]
bai$year <- as.integer(bai$year)
rownames(bai) <- NULL

# Climate -----------------------------------------------------------------

clim_files <- c(
  PRCP = "pr_peninsula_pred.csv",
  ETO = "eto_peninsula.csv",
  TMIN = "tmin_peninsula_pred.csv",
  TMAX = "tmax_peninsula_pred.csv"
)
clim <- lapply(clim_files, function(f) {
  d <- read.csv(file.path(raw_dir, "clima", f), check.names = FALSE)
  # 48 periods per year, starting on days 1, 9, 16 and 23 of each month
  stopifnot(
    nrow(d) == 48 * 60,
    d[1, 1] == "1961-01-01",
    d[nrow(d), 1] == "2020-12-23",
    all(substr(d[, 1], 9, 10) == rep(c("01", "09", "16", "23"), 720))
  )
  d$Tarazona
})
clim <- ts(round(do.call(cbind, clim), 2), start = c(1961, 1), frequency = 48)

# Assemble and save -------------------------------------------------------

tarazona <- list(
  site = list(
    name = meta[1],
    lat = latlon[1],
    lon = latlon[2],
    elevation = as.numeric(meta[2]),
    species = meta[4],
    plot = "TAPHD",
    treatment = "control (unthinned)",
    thinning.year = as.numeric(meta[5])
  ),
  bai = bai,
  clim = clim
)

save(tarazona, file = "data/tarazona.rda", compress = "xz")
