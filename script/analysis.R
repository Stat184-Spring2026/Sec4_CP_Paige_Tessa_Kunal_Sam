# ==============================================================================
# analysis.R — U.S. Housing Affordability (1993–2024)
# STAT 184 Section 4 — Paige, Tessa, Kunal, Sam
#
# Data sourced from FRED (Federal Reserve Economic Data):
#   MSPUS          Median Sales Price of Houses Sold (quarterly, $)
#   CUUR0000SEHA   CPI: Rent of Primary Residence (monthly index, 1982-84=100)
#   MEHOINUSA646N  Median Household Income (annual, $)
#   CPIAUCSL       CPI-U All Items (monthly index, 1982-84=100)
#
# CSVs downloaded directly from:
#   https://fred.stlouisfed.org/graph/fredgraph.csv?id=<SERIES_ID>
# ==============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(patchwork)

setwd("/home/claude/housing_project")
dir.create("plots", showWarnings = FALSE)

# ── Shared theme ──────────────────────────────────────────────────────────────
theme_housing <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(
      plot.title       = element_text(face = "bold", size = base + 1,
                                      margin = margin(b = 5)),
      plot.subtitle    = element_text(color = "grey40", size = base - 1),
      plot.caption     = element_text(color = "grey55", size = 8.5, hjust = 0,
                                      margin = margin(t = 8)),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92"),
      axis.title       = element_text(size = base - 1),
      legend.position  = "bottom",
      legend.title     = element_blank()
    )
}

CAP  <- paste0("Sources: FRED — MSPUS, CUUR0000SEHA, MEHOINUSA646N, CPIAUCSL",
               "\nSTAT 184 Section 4 | Paige, Tessa, Kunal, Sam")
XBRK <- seq(1993, 2024, by = 3)

BLUE  <- "#1B4F8A"
RUST  <- "#C1440E"
GREEN <- "#1F7A4B"
PURP  <- "#6B3FA0"
GOLD  <- "#B8860B"
TEAL  <- "#1A7A7A"

# ==============================================================================
# 1. LOAD & CLEAN DATA
# ==============================================================================

# ── Median home price (quarterly) ─────────────────────────────────────────────
home_q <- read_csv("data/MSPUS.csv", show_col_types = FALSE) %>%
  rename(date = observation_date, home_price = MSPUS) %>%
  mutate(date = ymd(date), year = year(date))

home_annual <- home_q %>%
  group_by(year) %>%
  summarise(median_home_price = mean(home_price, na.rm = TRUE), .groups = "drop")

# ── CPI rent component (monthly) ──────────────────────────────────────────────
rent_raw <- read_csv("data/CUUR0000SEHA.csv", show_col_types = FALSE) %>%
  rename(date = observation_date, rent_idx = CUUR0000SEHA) %>%
  mutate(date = ymd(date), year = year(date))

rent_annual <- rent_raw %>%
  group_by(year) %>%
  summarise(rent_idx = mean(rent_idx, na.rm = TRUE), .groups = "drop")

# Calibrate to Census 2000 median gross rent: $602/month
base_idx_2000 <- rent_annual %>% filter(year == 2000) %>% pull(rent_idx)
rent_annual   <- rent_annual %>%
  mutate(median_rent = rent_idx / base_idx_2000 * 602) %>%
  select(year, median_rent)

# ── Median household income (annual) ──────────────────────────────────────────
income_raw <- read_csv("data/MEHOINUSA646N.csv", show_col_types = FALSE) %>%
  rename(date = observation_date, median_hh_income_nom = MEHOINUSA646N) %>%
  mutate(year = year(ymd(date))) %>%
  select(year, median_hh_income_nom)

# ── CPI-U all items (monthly → annual) ────────────────────────────────────────
cpi_raw <- read_csv("data/CPIAUCSL.csv", show_col_types = FALSE) %>%
  rename(date = observation_date, cpi = CPIAUCSL) %>%
  mutate(date = ymd(date), year = year(date))

cpi_annual <- cpi_raw %>%
  group_by(year) %>%
  summarise(cpi = mean(cpi, na.rm = TRUE), .groups = "drop")

# ── Assemble master frame ─────────────────────────────────────────────────────
cpi_2024 <- cpi_annual %>% filter(year == 2024) %>% pull(cpi)

housing <- home_annual %>%
  left_join(rent_annual,  by = "year") %>%
  left_join(income_raw,   by = "year") %>%
  left_join(cpi_annual,   by = "year") %>%
  filter(year >= 1993, year <= 2024) %>%
  arrange(year) %>%
  mutate(
    # Real income in 2024 dollars
    median_hh_income_real = median_hh_income_nom * (cpi_2024 / cpi),
    # Price-to-rent ratio
    price_to_rent         = median_home_price / (median_rent * 12),
    # Price-to-income ratio
    price_to_income       = median_home_price / median_hh_income_nom,
    # YoY growth rates (%)
    income_growth_nom     = (median_hh_income_nom  / lag(median_hh_income_nom)  - 1) * 100,
    income_growth_real    = (median_hh_income_real / lag(median_hh_income_real) - 1) * 100,
    cpi_yoy               = (cpi / lag(cpi) - 1) * 100
  )

write_csv(housing, "data/housing_metrics_master.csv")
message("Master dataset: ", nrow(housing), " rows, ", ncol(housing), " cols")
print(housing %>% select(year, median_home_price, median_rent,
                          median_hh_income_nom, cpi, price_to_rent,
                          price_to_income) %>% as.data.frame())

# ==============================================================================
# 2. FIGURE 1 — Median Home Sales Price
# ==============================================================================
p1 <- ggplot(housing, aes(x = year, y = median_home_price)) +
  geom_area(fill = BLUE, alpha = 0.12) +
  geom_line(color = BLUE, linewidth = 1.1) +
  geom_point(color = BLUE, size = 2, shape = 21, fill = "white", stroke = 1.2) +
  annotate("rect", xmin = 2007.5, xmax = 2011.5,
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.15) +
  annotate("text", x = 2009.5, y = 490000,
           label = "Housing\ncrisis", color = "grey40", size = 3, hjust = 0.5) +
  annotate("rect", xmin = 2019.5, xmax = 2023,
           ymin = -Inf, ymax = Inf, fill = "#FFF3CD", alpha = 0.35) +
  annotate("text", x = 2021, y = 490000,
           label = "Pandemic\nboom", color = "#856404", size = 3, hjust = 0.5) +
  scale_x_continuous(breaks = XBRK) +
  scale_y_continuous(labels = label_dollar(scale = 1e-3, suffix = "K"),
                     limits = c(0, 540000)) +
  labs(title    = "Median U.S. Home Sales Price (1993–2024)",
       subtitle = "Nominal dollars · FRED: MSPUS (quarterly average)",
       x = "Year", y = "Median Sales Price", caption = CAP) +
  theme_housing()

ggsave("plots/01_median_home_price.png", p1, width = 9, height = 5.2, dpi = 150)
message("Saved plot 1")

# ==============================================================================
# 3. FIGURE 2 — Estimated Median Monthly Rent
# ==============================================================================
p2 <- ggplot(housing, aes(x = year, y = median_rent)) +
  geom_area(fill = RUST, alpha = 0.12) +
  geom_line(color = RUST, linewidth = 1.1) +
  geom_point(color = RUST, size = 2, shape = 21, fill = "white", stroke = 1.2) +
  scale_x_continuous(breaks = XBRK) +
  scale_y_continuous(labels = label_dollar(accuracy = 1)) +
  labs(title    = "Estimated Median U.S. Monthly Rent (1993–2024)",
       subtitle = "CPI rent index (CUUR0000SEHA) scaled to Census 2000 median gross rent ($602/mo)",
       x = "Year", y = "Estimated Monthly Rent", caption = CAP) +
  theme_housing()

ggsave("plots/02_median_rent.png", p2, width = 9, height = 5.2, dpi = 150)
message("Saved plot 2")

# ==============================================================================
# 4. FIGURE 3 — Price-to-Rent Ratio
# ==============================================================================
ptr_zones <- tibble(
  ymin  = c(0,  15, 20),
  ymax  = c(15, 20, 40),
  zone  = c("Buy favored (<15)", "Neutral (15–20)", "Rent favored (>20)"),
  fill  = c("#d4edda", "#fff3cd", "#f8d7da")
)

p3 <- ggplot() +
  geom_rect(data = ptr_zones,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
            fill = ptr_zones$fill, alpha = 0.4) +
  geom_line(data = housing, aes(x = year, y = price_to_rent),
            color = GOLD, linewidth = 1.3) +
  geom_point(data = housing, aes(x = year, y = price_to_rent),
             color = GOLD, size = 2.2) +
  annotate("text", x = 1994, y = 7.5,
           label = "Buy favored  (ratio < 15)", color = "#155724", size = 3.1, hjust = 0) +
  annotate("text", x = 1994, y = 17.3,
           label = "Neutral  (15–20)", color = "#856404", size = 3.1, hjust = 0) +
  annotate("text", x = 1994, y = 22.5,
           label = "Rent favored  (ratio > 20)", color = "#721c24", size = 3.1, hjust = 0) +
  scale_x_continuous(breaks = XBRK) +
  scale_y_continuous(breaks = seq(0, 40, by = 5)) +
  labs(title    = "Price-to-Rent Ratio (1993–2024)",
       subtitle = "Annual median home price ÷ (estimated monthly rent × 12)",
       x = "Year", y = "Price-to-Rent Ratio", caption = CAP) +
  theme_housing()

ggsave("plots/03_price_to_rent.png", p3, width = 9, height = 5.2, dpi = 150)
message("Saved plot 3")

# ==============================================================================
# 5. FIGURE 4 — Median Household Income: Nominal vs. Real
# ==============================================================================
income_long <- housing %>%
  select(year, median_hh_income_nom, median_hh_income_real) %>%
  pivot_longer(-year, names_to = "type", values_to = "income") %>%
  mutate(type = recode(type,
    median_hh_income_nom  = "Nominal",
    median_hh_income_real = "Real (2024 $)"))

p4 <- ggplot(income_long, aes(x = year, y = income,
                               color = type, linetype = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Nominal" = GREEN, "Real (2024 $)" = "#0A4F2E")) +
  scale_linetype_manual(values = c("Nominal" = "solid", "Real (2024 $)" = "dashed")) +
  scale_x_continuous(breaks = XBRK) +
  scale_y_continuous(labels = label_dollar(scale = 1e-3, suffix = "K")) +
  labs(title    = "Median U.S. Household Income: Nominal vs. Real (1993–2024)",
       subtitle = "Nominal = current dollars · Real = adjusted to 2024 dollars using CPI-U",
       x = "Year", y = "Median Household Income", caption = CAP) +
  theme_housing()

ggsave("plots/04_income_nominal_real.png", p4, width = 9, height = 5.2, dpi = 150)
message("Saved plot 4")

# ==============================================================================
# 6. FIGURE 5 — Nominal & Real Income Growth (YoY %)
# ==============================================================================
growth_long <- housing %>%
  filter(!is.na(income_growth_nom)) %>%
  select(year, income_growth_nom, income_growth_real) %>%
  pivot_longer(-year, names_to = "type", values_to = "pct") %>%
  mutate(type = recode(type,
    income_growth_nom  = "Nominal",
    income_growth_real = "Real (2024 $)"))

p5 <- ggplot(growth_long, aes(x = year, y = pct, fill = type)) +
  geom_col(position = "dodge", width = 0.75, alpha = 0.85) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "grey30") +
  scale_fill_manual(values = c("Nominal" = GREEN, "Real (2024 $)" = "#A8D5B5")) +
  scale_x_continuous(breaks = XBRK) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
  labs(title    = "Year-over-Year Household Income Growth: Nominal vs. Real (1994–2024)",
       subtitle = "Negative real growth = purchasing power loss",
       x = "Year", y = "YoY Growth (%)", caption = CAP) +
  theme_housing()

ggsave("plots/05_income_growth_yoy.png", p5, width = 9, height = 5.2, dpi = 150)
message("Saved plot 5")

# ==============================================================================
# 7. FIGURE 6 — CPI Level + Annual Inflation Rate (patchwork)
# ==============================================================================
p6a <- ggplot(housing, aes(x = year, y = cpi)) +
  geom_area(fill = PURP, alpha = 0.12) +
  geom_line(color = PURP, linewidth = 1.1) +
  scale_x_continuous(breaks = XBRK) +
  labs(title    = "CPI-U All Items Index (1993–2024)",
       subtitle = "Annual average · FRED: CPIAUCSL (base: 1982–84 = 100)",
       x = NULL, y = "CPI Index") +
  theme_housing() +
  theme(plot.caption = element_blank(), axis.text.x = element_blank())

p6b <- housing %>%
  filter(!is.na(cpi_yoy)) %>%
  ggplot(aes(x = year, y = cpi_yoy, fill = cpi_yoy > 2)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "grey40", linewidth = 0.7) +
  annotate("text", x = 1994, y = 2.35, hjust = 0,
           label = "Fed 2% target", color = "grey40", size = 3) +
  scale_fill_manual(values = c("TRUE" = PURP, "FALSE" = "#C0392B"), guide = "none") +
  scale_x_continuous(breaks = XBRK) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
  labs(title = NULL, subtitle = "Annual CPI inflation rate (%)",
       x = "Year", y = "Inflation (%)", caption = CAP) +
  theme_housing()

p6 <- p6a / p6b + plot_layout(heights = c(1, 1))

ggsave("plots/06_cpi_inflation.png", p6, width = 9, height = 8, dpi = 150)
message("Saved plot 6")

# ==============================================================================
# 8. FIGURE 7 — Price-to-Income Ratio
# ==============================================================================
p7 <- ggplot(housing, aes(x = year, y = price_to_income)) +
  geom_area(fill = TEAL, alpha = 0.10) +
  geom_line(color = TEAL, linewidth = 1.3) +
  geom_point(color = TEAL, size = 2.2) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_hline(yintercept = 5, linetype = "dotted", color = "#C0392B", linewidth = 0.8) +
  annotate("text", x = 1994, y = 3.18, hjust = 0,
           label = "Historical norm (~3×)", color = "grey40", size = 3) +
  annotate("text", x = 1994, y = 5.18, hjust = 0,
           label = "Affordability stress threshold (~5×)", color = "#C0392B", size = 3) +
  scale_x_continuous(breaks = XBRK) +
  scale_y_continuous(labels = label_number(suffix = "×", accuracy = 0.1)) +
  labs(title    = "Price-to-Income Ratio (1993–2024)",
       subtitle = "Median home sales price ÷ median household income (nominal)",
       x = "Year", y = "Price-to-Income Ratio", caption = CAP) +
  theme_housing()

ggsave("plots/07_price_to_income.png", p7, width = 9, height = 5.2, dpi = 150)
message("Saved plot 7")

# ==============================================================================
# 9. FIGURE 8 — All Metrics Indexed to 2000 = 100
# ==============================================================================
idx_df <- housing %>%
  filter(year >= 2000) %>%
  mutate(across(c(median_home_price, median_rent,
                  median_hh_income_nom, median_hh_income_real, cpi),
                ~ . / first(.) * 100)) %>%
  select(year, median_home_price, median_rent,
         median_hh_income_nom, median_hh_income_real, cpi) %>%
  pivot_longer(-year, names_to = "metric", values_to = "idx") %>%
  mutate(metric = recode(metric,
    median_home_price     = "Home Price",
    median_rent           = "Rent",
    median_hh_income_nom  = "Nominal Income",
    median_hh_income_real = "Real Income",
    cpi                   = "CPI"))

palette8 <- c(
  "Home Price"    = BLUE,
  "Rent"          = RUST,
  "Nominal Income"= GREEN,
  "Real Income"   = "#0A4F2E",
  "CPI"           = PURP
)
ltype8 <- c(
  "Home Price" = "solid", "Rent" = "solid",
  "Nominal Income" = "solid", "Real Income" = "dashed", "CPI" = "solid"
)

p8 <- ggplot(idx_df, aes(x = year, y = idx, color = metric, linetype = metric)) +
  geom_hline(yintercept = 100, color = "grey70", linetype = "dotted") +
  geom_line(linewidth = 1.15) +
  annotate("text", x = 2001, y = 103, hjust = 0, color = "grey55", size = 2.9,
           label = "Baseline = 100 (year 2000)") +
  scale_color_manual(values = palette8) +
  scale_linetype_manual(values = ltype8) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 4)) +
  scale_y_continuous(breaks = seq(50, 350, by = 50),
                     labels = label_number(suffix = "")) +
  labs(title    = "Housing Costs, Income & Inflation — All Indexed to Year 2000",
       subtitle = "Each series set to 100 in 2000; divergence reveals relative affordability loss",
       x = "Year", y = "Index (2000 = 100)", caption = CAP) +
  guides(color    = guide_legend(nrow = 2),
         linetype = guide_legend(nrow = 2)) +
  theme_housing()

ggsave("plots/08_all_indexed.png", p8, width = 10, height = 5.8, dpi = 150)
message("Saved plot 8")

message("\nAll plots written to plots/")
