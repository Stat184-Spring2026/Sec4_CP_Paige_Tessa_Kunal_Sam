# ==============================================================================
# 01_fetch_data.R
# Housing Affordability Analysis — STAT 184 Course Project
# Authors: Paige, Tessa, Kunal, Sam
#
# Fetches all required series from FRED (Federal Reserve Economic Data)
# using their public API (no key required for the observation endpoint).
#
# FRED Series IDs used:
#   MSPUS  — Median Sales Price of Houses Sold (quarterly)
#   ZORI   — Zillow Observed Rent Index, all homes (monthly) [FRED mirror]
#   MEHOINUSA646N — Real Median Household Income (annual)
#   MHINUS — Median Household Income in the US (nominal, annual)
#   CPIAUCSL — CPI for All Urban Consumers (monthly)
#   CPILFESL — Core CPI (ex food & energy, monthly)
#
# Note on rent: FRED's ZORI series starts 2015. For longer history we also
# pull CUUR0000SEHA (Rent of primary residence, CPI component) as a proxy.
# ==============================================================================

library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

# ── Helper: pull one FRED series ──────────────────────────────────────────────
fetch_fred <- function(series_id,
                       start_date = "1993-01-01",
                       end_date   = "2024-12-31") {
  base <- "https://api.stlouisfed.org/fred/series/observations"
  # FRED public endpoint — no API key needed for this call format
  url <- paste0(
    base,
    "?series_id=", series_id,
    "&observation_start=", start_date,
    "&observation_end=",   end_date,
    "&file_type=json"
  )
  resp <- GET(url)
  if (http_error(resp)) {
    warning(paste("Failed to fetch:", series_id))
    return(NULL)
  }
  raw <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
  df  <- as_tibble(raw$observations) %>%
    select(date, value) %>%
    mutate(
      date  = ymd(date),
      value = suppressWarnings(as.numeric(value)),  # "." → NA
      series = series_id
    ) %>%
    filter(!is.na(value))
  df
}

# ── Fetch all series ───────────────────────────────────────────────────────────
message("Fetching FRED data…")

home_price_raw   <- fetch_fred("MSPUS")          # Median sales price, quarterly
rent_cpi_raw     <- fetch_fred("CUUR0000SEHA")   # CPI rent component, monthly index
income_nominal   <- fetch_fred("MEHOINUSA646N")  # Median HH income nominal (BLS/Census)
income_real      <- fetch_fred("MHINUS")         # Alternative nominal series
cpi_all_raw      <- fetch_fred("CPIAUCSL")       # CPI all items, monthly
cpi_core_raw     <- fetch_fred("CPILFESL")       # Core CPI, monthly

# ── Annual averages helper ────────────────────────────────────────────────────
to_annual <- function(df, value_col = "value") {
  df %>%
    mutate(year = year(date)) %>%
    group_by(year, series) %>%
    summarise(value = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
}

# ── Home price (already quarterly → annual) ───────────────────────────────────
home_price <- home_price_raw %>%
  to_annual() %>%
  rename(median_home_price = value) %>%
  select(year, median_home_price)

# ── CPI all items (annual average, 1982-84 = 100) ────────────────────────────
cpi_annual <- cpi_all_raw %>%
  to_annual() %>%
  rename(cpi = value) %>%
  select(year, cpi)

# ── Rent CPI component → convert index to approximate $rent ───────────────────
# Base rent index 2000 = 100; median US rent 2000 ≈ $602/mo (Census)
rent_base_2000 <- 602
rent_cpi_annual <- rent_cpi_raw %>%
  to_annual() %>%
  rename(rent_idx = value) %>%
  select(year, rent_idx)

rent_base_idx <- rent_cpi_annual %>% filter(year == 2000) %>% pull(rent_idx)

rent_annual <- rent_cpi_annual %>%
  mutate(median_rent = rent_idx / rent_base_idx * rent_base_2000) %>%
  select(year, median_rent, rent_idx)

# ── Median household income (nominal) ────────────────────────────────────────
income_nom <- income_nominal %>%
  mutate(year = year(date)) %>%
  rename(median_hh_income_nominal = value) %>%
  select(year, median_hh_income_nominal)

# ── CPI base year for real conversion (using 2024 dollars) ───────────────────
cpi_2024 <- cpi_annual %>% filter(year == 2024) %>% pull(cpi)

# ── Assemble master data frame ────────────────────────────────────────────────
housing_df <- home_price %>%
  left_join(rent_annual,  by = "year") %>%
  left_join(income_nom,   by = "year") %>%
  left_join(cpi_annual,   by = "year") %>%
  filter(year >= 1993, year <= 2024) %>%
  arrange(year) %>%
  # Derived metrics
  mutate(
    # Price-to-rent ratio (annual price / (monthly rent × 12))
    price_to_rent = median_home_price / (median_rent * 12),

    # Real income (2024 dollars)
    median_hh_income_real = median_hh_income_nominal * (cpi_2024 / cpi),

    # Price-to-income ratio
    price_to_income = median_home_price / median_hh_income_nominal,

    # Nominal income YoY growth (%)
    income_growth_nominal = (median_hh_income_nominal /
                               lag(median_hh_income_nominal) - 1) * 100,

    # Real income YoY growth (%)
    income_growth_real = (median_hh_income_real /
                            lag(median_hh_income_real) - 1) * 100,

    # CPI YoY inflation (%)
    cpi_yoy = (cpi / lag(cpi) - 1) * 100
  )

message("Master data frame built: ", nrow(housing_df), " rows × ", ncol(housing_df), " cols")
glimpse(housing_df)

# ── Save to CSV ───────────────────────────────────────────────────────────────
write_csv(housing_df, "data/housing_metrics_master.csv")
message("Saved → data/housing_metrics_master.csv")
