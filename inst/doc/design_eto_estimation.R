## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(BrazilMet)

## -----------------------------------------------------------------------------
see_stations_info()

## ----download, eval = FALSE---------------------------------------------------
# df <- download_AWS_INMET_daily(
#   stations   = "A001",
#   start_date = "2000-01-01",
#   end_date   = "2025-03-31"
# )

## ----load-data----------------------------------------------------------------
df <- readRDS(system.file("extdata", "A001_daily_2000_2025.rds", package = "BrazilMet"))
df$date <- as.Date(df$date)

## ----gap-fill-----------------------------------------------------------------
df <- fill_gaps(df, method = "both", max_gap = 3)

## -----------------------------------------------------------------------------
df$eto <- daily_eto_FAO56(
  lat    = df$latitude_degrees,
  tmin   = df$tair_min_c,
  tmax   = df$tair_max_c,
  tmean  = df$tair_mean_c,
  Rs     = df$sr_mj_m2,
  u2     = df$ws_2_m_s,
  Patm   = df$patm_mb,
  RH_max = df$rh_max_porc,
  RH_min = df$rh_min_porc,
  z      = df$altitude_m,
  date   = df$date
)

## -----------------------------------------------------------------------------
# Ensure date column is in Date format
df$date <- as.Date(df$date)

eto_design <- BrazilMet::design_eto(eto_daily_data = df, percentile = .80)


## ----plot-eto-ggplot, fig.width = 10, fig.height = 4--------------------------

print(eto_design)


