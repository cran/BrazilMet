## ----include = FALSE----------------------------------------------------------
Sys.setlocale("LC_TIME", "C") # English month labels in the plots
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 10,
  fig.height = 4.5,
  fig.align = "center",
  dpi = 150,
  out.width = "100%"
)

## ----load-packages------------------------------------------------------------
library(BrazilMet)
library(ggplot2)

## ----theme, include = FALSE---------------------------------------------------
# Shared palette (colour-blind friendly) and plot theme
col_rain   <- "#0072B2"
col_eto    <- "#D55E00"
col_storage <- "#009E73"
col_deficit <- "#D55E00"
col_excess  <- "#0072B2"

theme_bm <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 15),
      plot.subtitle    = element_text(colour = "grey35"),
      plot.caption     = element_text(colour = "grey45", hjust = 0),
      panel.grid.minor = element_blank(),
      legend.position  = "top",
      legend.title     = element_blank(),
      axis.title       = element_text(colour = "grey25")
    )
}

## ----download, eval = FALSE---------------------------------------------------
# df <- download_AWS_INMET_daily(
#   stations   = "A001",
#   start_date = "2023-01-01",
#   end_date   = "2024-12-31"
# )

## ----load-data----------------------------------------------------------------
df <- readRDS(system.file("extdata", "A001_daily_2023_2024.rds", package = "BrazilMet"))
df$date <- as.Date(df$date)

df[1:5, c("station_code", "date", "tair_mean_c", "rainfall_mm", "sr_mj_m2")]

## ----count-na-----------------------------------------------------------------
eto_vars <- c("tair_mean_c", "tair_min_c", "tair_max_c", "rh_max_porc",
              "rh_min_porc", "ws_2_m_s", "patm_mb", "sr_mj_m2")

colSums(is.na(df[eto_vars]))

## ----gap-fill-----------------------------------------------------------------
df <- fill_gaps(df, vars = eto_vars, max_gap = 3)

colSums(is.na(df[eto_vars]))

## ----eto----------------------------------------------------------------------
df$eto <- round(daily_eto_FAO56(
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
), 1)

anyNA(df$eto)

## ----plot-ppt-eto, fig.height = 5---------------------------------------------
ggplot(df, aes(x = date)) +
  geom_col(aes(y = rainfall_mm, fill = "Rainfall"), width = 1) +
  geom_line(aes(y = eto, colour = "ETo (FAO-56)"), linewidth = 0.5) +
  scale_fill_manual(values = c("Rainfall" = col_rain)) +
  scale_colour_manual(values = c("ETo (FAO-56)" = col_eto)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y", expand = c(0, 0)) +
  labs(
    title    = "Daily rainfall and reference evapotranspiration",
    subtitle = "INMET station A001 - Brasilia, DF",
    x        = NULL,
    y        = "mm / day",
    caption  = "Source: INMET. ETo calculated with FAO-56 Penman-Monteith."
  ) +
  theme_bm()

## ----daily-balance------------------------------------------------------------
bal_daily <- water_balance(
  ppt       = df$rainfall_mm,
  etp       = df$eto,
  AWC       = 100,
  period    = df$date,
  time_step = "daily"
)

head(bal_daily)

## ----plot-storage-------------------------------------------------------------
ggplot(bal_daily, aes(x = period, y = awc_arm)) +
  geom_area(fill = col_storage, alpha = 0.25) +
  geom_line(colour = col_storage, linewidth = 0.7) +
  geom_hline(yintercept = 50, linetype = "dashed", colour = "grey40") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y", expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Soil water storage",
    subtitle = "Percentage of the available water capacity (AWC = 100 mm)",
    x        = NULL,
    y        = "Soil water storage (% of AWC)",
    caption  = "Dashed line: 50% of AWC."
  ) +
  theme_bm()

## ----monthly-balance----------------------------------------------------------
df$month <- as.Date(format(df$date, "%Y-%m-01"))

monthly <- aggregate(cbind(rainfall_mm, eto) ~ month, data = df, FUN = sum)

bal_monthly <- water_balance(
  ppt       = monthly$rainfall_mm,
  etp       = monthly$eto,
  AWC       = 100,
  period    = monthly$month,
  time_step = "monthly"
)

head(round(bal_monthly[, c("ppt", "etp", "arm", "etr", "def", "exc")], 1), 6)

## ----plot-monthly, fig.height = 5---------------------------------------------
ggplot(bal_monthly, aes(x = period)) +
  geom_col(aes(y = exc, fill = "Excess"), width = 25) +
  geom_col(aes(y = -def, fill = "Deficit"), width = 25) +
  geom_hline(yintercept = 0, colour = "grey30") +
  scale_fill_manual(values = c("Excess" = col_excess, "Deficit" = col_deficit)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y") +
  labs(
    title    = "Monthly water excess and deficit",
    subtitle = "Thornthwaite-Mather balance, AWC = 100 mm",
    x        = NULL,
    y        = "mm / month",
    caption  = "Source: INMET station A001."
  ) +
  theme_bm()

## ----annual-summary-----------------------------------------------------------
bal_monthly$year <- format(bal_monthly$period, "%Y")

annual <- aggregate(cbind(ppt, etp, etr, def, exc) ~ year, data = bal_monthly, FUN = sum)
names(annual) <- c("Year", "Rainfall", "ETo", "ETR", "Deficit", "Excess")

knitr::kable(annual, digits = 0, caption = "Annual water balance components (mm)")

## ----awc-comparison-----------------------------------------------------------
awc_values <- c(50, 100, 150)

bal_awc <- do.call(rbind, lapply(awc_values, function(awc) {
  b <- water_balance(
    ppt       = df$rainfall_mm,
    etp       = df$eto,
    AWC       = awc,
    period    = df$date,
    time_step = "daily"
  )
  b$AWC <- factor(paste("AWC =", awc, "mm"), levels = paste("AWC =", awc_values, "mm"))
  b
}))

## ----plot-awc, fig.height = 5-------------------------------------------------
ggplot(bal_awc, aes(x = period, y = awc_arm, colour = AWC)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c("#E69F00", "#0072B2", "#009E73")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y", expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Soil water storage for different AWC values",
    subtitle = "Soils with lower AWC dry out faster at the start of the dry season",
    x        = NULL,
    y        = "Soil water storage (% of AWC)",
    caption  = "Source: INMET station A001."
  ) +
  theme_bm()

## ----deficit-awc--------------------------------------------------------------
total_deficit <- aggregate(def ~ AWC, data = bal_awc, FUN = sum)
names(total_deficit) <- c("Soil", "Total deficit (mm)")

knitr::kable(total_deficit, caption = "Total water deficit in the period, by AWC")

