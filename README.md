# U.S. Housing Affordability (1993–2024)
## STAT 184 · Section 4 Course Project
**Authors:** Paige · Tessa · Kunal · Sam

---

## Overview
This project analyzes how U.S. housing costs have evolved over 31 years relative
to household incomes and general inflation, using four datasets downloaded directly
from **FRED** (Federal Reserve Economic Data) as CSV files.

## Repository Structure
```
.
├── analysis.R                    # Wrangles CSVs → 8 PNG plots
├── report.Rmd                    # Full R Markdown report (knit → report.html)
├── report.html                   # Compiled HTML report (open in browser)
├── data/
│   ├── MSPUS.csv                 # Median home sales price (quarterly)
│   ├── CUUR0000SEHA.csv          # CPI rent index (monthly)
│   ├── MEHOINUSA646N.csv         # Median household income (annual)
│   ├── CPIAUCSL.csv              # CPI-U all items (monthly)
│   └── housing_metrics_master.csv  # Generated master dataset
└── plots/
    ├── 01_median_home_price.png
    ├── 02_median_rent.png
    ├── 03_price_to_rent.png
    ├── 04_income_nominal_real.png
    ├── 05_income_growth_yoy.png
    ├── 06_cpi_inflation.png
    ├── 07_price_to_income.png
    └── 08_all_indexed.png
```

## Data Sources (FRED)
| File | Series | Description |
|------|--------|-------------|
| `MSPUS.csv` | MSPUS | Median Sales Price of Houses Sold |
| `CUUR0000SEHA.csv` | CUUR0000SEHA | CPI: Rent of Primary Residence |
| `MEHOINUSA646N.csv` | MEHOINUSA646N | Median Household Income |
| `CPIAUCSL.csv` | CPIAUCSL | CPI-U All Items |

Downloaded from: `https://fred.stlouisfed.org/graph/fredgraph.csv?id=<SERIES_ID>`

## How to Run

### Install packages (once)
```r
install.packages(c("readr","dplyr","tidyr","lubridate",
                   "ggplot2","scales","patchwork","rmarkdown","knitr"))
```

### Generate plots
```r
source("analysis.R")   # writes plots/ and data/housing_metrics_master.csv
```

### Compile full report
```r
rmarkdown::render("report.Rmd", output_file = "report.html")
# or: click Knit in RStudio
```

## Key Findings
| Metric | 1993 | 2024 | Change |
|--------|------|------|--------|
| Median home price | $151K | $477K | +217% |
| Estimated monthly rent | $455 | $1,876 | +312% |
| Nominal household income | $31,241 | $80,610 | +158% |
| Real household income (2024$) | $67,836 | $80,610 | +19% |
| Price-to-income ratio | 4.83× | 5.92× | — |
| Peak inflation | — | 7.96% (2022) | — |
