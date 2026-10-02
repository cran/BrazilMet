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
#   stations   = c("A001"),
#   start_date = "2023-01-01",
#   end_date   = "2024-12-31"
# )

## ----load-data----------------------------------------------------------------
df <- readRDS(system.file("extdata", "A001_daily_2023_2024.rds", package = "BrazilMet"))

## ----gap-fill-----------------------------------------------------------------
df <- fill_gaps(df, max_gap = 3)

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

## ----plot-eto-ggplot, fig.width = 10, fig.height = 4--------------------------

library(ggplot2)

# Ensure date column is in Date format
df$date <- as.Date(df$date)

ggplot(df, aes(x = date, y = eto)) +
  geom_line(color = "darkblue", linewidth = 1) +
  labs(
    title = "Reference Evapotranspiration (FAO-56)",
    x = "Date",
    y = "ETo (mm/day)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )


