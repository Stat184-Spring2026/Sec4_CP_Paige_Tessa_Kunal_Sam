# ==============================================================================
# 02_visualize.R
# Housing Affordability Analysis — STAT 184 Course Project
# Authors: Paige, Tessa, Kunal, Sam
#
# Produces all eight figures described in the project README.
# Run after 01_fetch_data.R (or source it inline below).
# ==============================================================================

library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

# ── Load data ─────────────────────────────────────────────────────────────────
# If running standalone, source the fetch script first:
# source("01_fetch_data.R")
# Otherwise load the saved CSV:
housing_df <- read_csv("data/housing_metrics_master.csv", show_col_types = FALSE)

# ── Shared theme ──────────────────────────────────────────────────────────────
theme_housing <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 14, margin = margin(b = 6)),
      plot.subtitle = element_text(color = "grey40", size = 11),
      plot.caption  = element_text(color = "grey55", size = 9, hjust = 0),
      panel.grid.minor = element_blank(),
      axis.title    = element_text(size = 11),
      legend.position = "bottom"
    )
}

CAPTION <- "Sources: FRED (MSPUS, CUUR0000SEHA, MEHOINUSA646N, CPIAUCSL) | STAT 184 — Paige, Tessa, Kunal, Sam"

# ── Palette ───────────────────────────────────────────────────────────────────
COL_PRICE   <- "#1B4F8A"
COL_RENT    <- "#E8773A"
COL_INCOME  <- "#2E8B57"
COL_CPI     <- "#8B2E6A"
COL_RATIO   <- "#B8860B"
COL_GROWTH  <- "#3A7DC9"


# ==============================================================================
# FIGURE 1 — Median Home Price (1993–2024)
# ==============================================================================
p1 <- ggplot(housing_df, aes(x = year, y = median_home_price)) +
  geom_area(fill = COL_PRICE, alpha = 0.15) +
  geom_line(color = COL_PRICE, linewidth = 1.2) +
  geom_point(color = COL_PRICE, size = 2) +
  scale_y_continuous(labels = label_dollar(scale = 1e-3, suffix = "K")) +
  scale_x_continuous(breaks = seq(1993, 2024, by = 5)) +
  labs(
    title    = "Median U.S. Home Sales Price (1993–2024)",
    subtitle = "Nominal dollars — FRED series MSPUS",
    x = "Year", y = "Median Sales Price",
    caption  = CAPTION
  ) +
  theme_housing()

ggsave("plots/01_median_home_price.png", p1, width = 9, height = 5, dpi = 150)
message("Saved: plots/01_median_home_price.png")


# ==============================================================================
# FIGURE 2 — Median Rent (estimated from CPI rent component)
# ==============================================================================
p2 <- ggplot(housing_df, aes(x = year, y = median_rent)) +
  geom_area(fill = COL_RENT, alpha = 0.15) +
  geom_line(color = COL_RENT, linewidth = 1.2) +
  geom_point(color = COL_RENT, size = 2) +
  scale_y_continuous(labels = label_dollar()) +
  scale_x_continuous(breaks = seq(1993, 2024, by = 5)) +
  labs(
    title    = "Estimated Median U.S. Monthly Rent (1993–2024)",
    subtitle = "Indexed from CPI rent component (CUUR0000SEHA), anchored to Census 2000 median ($602/mo)",
    x = "Year", y = "Estimated Monthly Rent",
    caption  = CAPTION
  ) +
  theme_housing()

ggsave("plots/02_median_rent.png", p2, width = 9, height = 5, dpi = 150)
message("Saved: plots/02_median_rent.png")


# ==============================================================================
# FIGURE 3 — Price-to-Rent Ratio
# ==============================================================================
# Add reference bands for the conventional "buy vs rent" thresholds
ptr_breaks <- tibble(
  ymin  = c(1,  15, 20),
  ymax  = c(15, 20, 50),
  label = c("Buy favored\n(<15)", "Neutral\n(15–20)", "Rent favored\n(>20)"),
  fill  = c("#d4edda", "#fff3cd", "#f8d7da")
)

p3 <- ggplot() +
  geom_rect(
    data = ptr_breaks,
    aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = fill),
    alpha = 0.25
  ) +
  scale_fill_identity() +
  geom_line(data = housing_df, aes(x = year, y = price_to_rent),
            color = COL_RATIO, linewidth = 1.4) +
  geom_point(data = housing_df, aes(x = year, y = price_to_rent),
             color = COL_RATIO, size = 2.2) +
  scale_x_continuous(breaks = seq(1993, 2024, by = 5)) +
  labs(
    title    = "Price-to-Rent Ratio (1993–2024)",
    subtitle = "Annual home price ÷ (monthly rent × 12) | Green = buy favored · Red = rent favored",
    x = "Year", y = "Price-to-Rent Ratio",
    caption  = CAPTION
  ) +
  theme_housing()

ggsave("plots/03_price_to_rent.png", p3, width = 9, height = 5, dpi = 150)
message("Saved: plots/03_price_to_rent.png")


# ==============================================================================
# FIGURE 4 — Median Household Income: Nominal vs Real (2024 $)
# ==============================================================================
income_long <- housing_df %>%
  select(year, median_hh_income_nominal, median_hh_income_real) %>%
  pivot_longer(
    cols      = c(median_hh_income_nominal, median_hh_income_real),
    names_to  = "type",
    values_to = "income"
  ) %>%
  mutate(type = recode(type,
    median_hh_income_nominal = "Nominal",
    median_hh_income_real    = "Real (2024 $)"
  ))

p4 <- ggplot(income_long, aes(x = year, y = income, color = type, linetype = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Nominal" = COL_INCOME, "Real (2024 $)" = "#145A32")) +
  scale_linetype_manual(values = c("Nominal" = "solid", "Real (2024 $)" = "dashed")) +
  scale_y_continuous(labels = label_dollar(scale = 1e-3, suffix = "K")) +
  scale_x_continuous(breaks = seq(1993, 2024, by = 5)) +
  labs(
    title    = "Median U.S. Household Income: Nominal vs. Real (1993–2024)",
    subtitle = "Real income adjusted to 2024 dollars using CPI-U (CPIAUCSL)",
    x = "Year", y = "Median Household Income",
    color = NULL, linetype = NULL,
    caption  = CAPTION
  ) +
  theme_housing()

ggsave("plots/04_median_income_nominal_real.png", p4, width = 9, height = 5, dpi = 150)
message("Saved: plots/04_median_income_nominal_real.png")


# ==============================================================================
# FIGURE 5 — Nominal & Real Income Growth (YoY %)
# ==============================================================================
growth_long <- housing_df %>%
  select(year, income_growth_nominal, income_growth_real) %>%
  filter(!is.na(income_growth_nominal)) %>%
  pivot_longer(
    cols      = c(income_growth_nominal, income_growth_real),
    names_to  = "type",
    values_to = "growth_pct"
  ) %>%
  mutate(type = recode(type,
    income_growth_nominal = "Nominal",
    income_growth_real    = "Real (2024 $)"
  ))

p5 <- ggplot(growth_long, aes(x = year, y = growth_pct, fill = type)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  scale_fill_manual(values = c("Nominal" = COL_GROWTH, "Real (2024 $)" = "#A9CCE3")) +
  scale_x_continuous(breaks = seq(1993, 2024, by = 5)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Year-over-Year Income Growth: Nominal vs. Real (1994–2024)",
    subtitle = "Percent change from prior year | Negative real growth = purchasing power loss",
    x = "Year", y = "YoY Growth (%)",
    fill = NULL,
    caption  = CAPTION
  ) +
  theme_housing()

ggsave("plots/05_income_growth_yoy.png", p5, width = 9, height = 5, dpi = 150)
message("Saved: plots/05_income_growth_yoy.png")


# ==============================================================================
# FIGURE 6 — CPI (All Items) and Inflation Rate
# ==============================================================================
p6a <- ggplot(housing_df, aes(x = year, y = cpi)) +
  geom_line(color = COL_CPI, linewidth = 1.2) +
  geom_area(fill = COL_CPI, alpha = 0.12) +
  scale_x_continuous(breaks = seq(1993, 2024, by = 5)) +
  labs(
    title    = "Consumer Price Index — All Urban Consumers (1993–2024)",
    subtitle = "FRED series CPIAUCSL (1982–84 = 100)",
    x = "Year", y = "CPI Index Value"
  ) +
  theme_housing() +
  theme(plot.caption = element_blank())

p6b <- ggplot(housing_df %>% filter(!is.na(cpi_yoy)), aes(x = year, y = cpi_yoy)) +
  geom_col(aes(fill = cpi_yoy > 0), show.legend = FALSE, alpha = 0.85) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "grey40") +
  annotate("text", x = 1994, y = 2.4, label = "Fed 2% target",
           color = "grey40", size = 3.2, hjust = 0) +
  scale_fill_manual(values = c("TRUE" = COL_CPI, "FALSE" = "#C0392B")) +
  scale_x_continuous(breaks = seq(1993, 2024, by = 5)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Annual CPI Inflation Rate (1994–2024)",
    subtitle = "Year-over-year % change in CPI-U",
    x = "Year", y = "Inflation (%)",
    caption  = CAPTION
  ) +
  theme_housing()

p6 <- p6a / p6b

ggsave("plots/06_cpi_and_inflation.png", p6, width = 9, height = 8, dpi = 150)
message("Saved: plots/06_cpi_and_inflation.png")


# ==============================================================================
# FIGURE 7 — Price-to-Income Ratio
# ==============================================================================
p7 <- ggplot(housing_df, aes(x = year, y = price_to_income)) +
  geom_line(color = COL_RATIO, linewidth = 1.4) +
  geom_point(color = COL_RATIO, size = 2.2) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 5, linetype = "dotted", color = "#C0392B") +
  annotate("text", x = 1994, y = 3.2, label = "Historical norm (~3×)",
           color = "grey40", size = 3.2, hjust = 0) +
  annotate("text", x = 1994, y = 5.2, label = "Affordability crisis threshold (~5×)",
           color = "#C0392B", size = 3.2, hjust = 0) +
  scale_x_continuous(breaks = seq(1993, 2024, by = 5)) +
  scale_y_continuous(labels = label_number(suffix = "×")) +
  labs(
    title    = "Price-to-Income Ratio (1993–2024)",
    subtitle = "Median home sales price ÷ median household income",
    x = "Year", y = "Price-to-Income Ratio",
    caption  = CAPTION
  ) +
  theme_housing()

ggsave("plots/07_price_to_income.png", p7, width = 9, height = 5, dpi = 150)
message("Saved: plots/07_price_to_income.png")


# ==============================================================================
# FIGURE 8 — Combined Dashboard: All key metrics indexed (2000 = 100)
# ==============================================================================
base_year <- 2000

index_df <- housing_df %>%
  filter(year >= base_year) %>%
  mutate(
    idx_home_price = median_home_price / first(median_home_price) * 100,
    idx_rent       = median_rent       / first(median_rent)       * 100,
    idx_income_nom = median_hh_income_nominal / first(median_hh_income_nominal) * 100,
    idx_income_real= median_hh_income_real    / first(median_hh_income_real)    * 100,
    idx_cpi        = cpi / first(cpi) * 100
  ) %>%
  select(year, idx_home_price, idx_rent, idx_income_nom, idx_income_real, idx_cpi) %>%
  pivot_longer(-year, names_to = "metric", values_to = "index") %>%
  mutate(metric = recode(metric,
    idx_home_price  = "Home Price",
    idx_rent        = "Rent",
    idx_income_nom  = "Nominal Income",
    idx_income_real = "Real Income",
    idx_cpi         = "CPI"
  ))

palette_dashboard <- c(
  "Home Price"    = COL_PRICE,
  "Rent"          = COL_RENT,
  "Nominal Income"= COL_INCOME,
  "Real Income"   = "#145A32",
  "CPI"           = COL_CPI
)

p8 <- ggplot(index_df, aes(x = year, y = index, color = metric, linewidth = metric)) +
  geom_line() +
  geom_hline(yintercept = 100, color = "grey70", linetype = "dotted") +
  annotate("text", x = 2001, y = 102, label = "Base = 100 (year 2000)",
           color = "grey55", size = 3, hjust = 0) +
  scale_color_manual(values = palette_dashboard) +
  scale_linewidth_manual(
    values = c("Home Price" = 1.4, "Rent" = 1.2, "Nominal Income" = 1.2,
               "Real Income" = 1.2, "CPI" = 0.9),
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 4)) +
  scale_y_continuous(labels = label_number(suffix = "")) +
  labs(
    title    = "Housing Cost vs. Income vs. Inflation: Indexed to Year 2000",
    subtitle = "All series set to 100 in 2000 — shows relative growth since baseline",
    x = "Year", y = "Index (2000 = 100)",
    color = NULL,
    caption  = CAPTION
  ) +
  theme_housing()

ggsave("plots/08_all_metrics_indexed.png", p8, width = 10, height = 6, dpi = 150)
message("Saved: plots/08_all_metrics_indexed.png")

message("\nAll 8 figures saved to plots/")
