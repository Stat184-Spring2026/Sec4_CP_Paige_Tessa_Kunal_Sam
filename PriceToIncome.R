# Setup ----
# Style Guide: Tidyverse Style Guide (https://style.tidyverse.org/)
# Primary Author: [Your Name] | Reviewed by: [Teammate Name]

library(tidyverse)
library(scales)

# Data source: FRED, Federal Reserve Bank of St. Louis.
# MSPUS = Median Sales Price of Houses Sold for the United States
# MEHOINUSA646N = Median Household Income in the United States

home_prices <- read_csv(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=MSPUS",
  show_col_types = FALSE
) |>
  rename(date = observation_date, median_home_price = MSPUS) |>
  mutate(
    date = ymd(date),
    year = year(date)
  ) |>
  filter(year >= 1995, year <= 2024) |>
  group_by(year) |>
  summarize(
    median_home_price = mean(median_home_price, na.rm = TRUE),
    .groups = "drop"
  )

income <- read_csv(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=MEHOINUSA646N",
  show_col_types = FALSE
) |>
  rename(date = observation_date, median_household_income = MEHOINUSA646N) |>
  mutate(
    date = ymd(date),
    year = year(date)
  ) |>
  filter(year >= 1995, year <= 2024) |>
  select(year, median_household_income)

price_income_data <- home_prices |>
  inner_join(income, by = "year") |>
  mutate(
    price_to_income_ratio = median_home_price / median_household_income
  )

ggplot(price_income_data, aes(x = year, y = price_to_income_ratio)) +
  annotate(
    "rect",
    xmin = 2007.5, xmax = 2009.5,
    ymin = -Inf,   ymax = Inf,
    fill = "#d73027", alpha = 0.12
  ) +
  annotate(
    "rect",
    xmin = 2019.5, xmax = 2022.5,
    ymin = -Inf,   ymax = Inf,
    fill = "#fc8d59", alpha = 0.12
  ) +
  geom_hline(
    yintercept = mean(price_income_data$price_to_income_ratio),
    linetype   = "dashed",
    color      = "gray50",
    linewidth  = 0.6
  ) +
  geom_line(color = "#2166ac", linewidth = 1.1) +
  geom_point(color = "#2166ac", size = 2.2) +
  geom_vline(xintercept = 2008, linetype = "dotted",
             color = "#d73027", linewidth = 0.8) +
  geom_vline(xintercept = 2021, linetype = "dotted",
             color = "#fc8d59", linewidth = 0.8) +
  annotate("text", x = 2008, y = max(price_income_data$price_to_income_ratio) * 1.025,
           label = "2008\nCrisis", hjust = 1.1, size = 3, color = "#d73027") +
  annotate("text", x = 2021, y = max(price_income_data$price_to_income_ratio) * 1.025,
           label = "COVID-19\nSpike", hjust = -0.1, size = 3, color = "#b35806") +
  annotate("text",
           x     = 1996,
           y     = mean(price_income_data$price_to_income_ratio),
           label = "Historical avg.",
           vjust = -0.5, size = 2.8, color = "gray40") +
  scale_x_continuous(breaks = seq(1995, 2025, by = 5)) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1),
    limits = c(NA, max(price_income_data$price_to_income_ratio) * 1.04)
  ) +
  labs(
    title    = "U.S. Home Prices Have Grown Faster Than Household Income",
    subtitle = "Price-to-income ratio: median home sale price divided by median household income, 1995–2024",
    x        = "Year",
    y        = "Price-to-Income Ratio (unitless)",
    caption  = "Source: Federal Reserve Bank of St. Louis (FRED).\nSeries MSPUS (median home price) and MEHOINUSA646N (median household income)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, color = "gray30"),
    plot.caption     = element_text(size = 8,  color = "gray40", hjust = 0),
    panel.grid.minor = element_blank()
  )
