library(readr)
library(ggplot2)

QUSR628BIS <- read_csv("QUSR628BIS.csv", show_col_types = FALSE)

year_data <- aggregate(
  QUSR628BIS$QUSR628BIS,
  by = list(year = format(QUSR628BIS$observation_date, "%Y")),
  FUN = mean
)

colnames(year_data) <- c("year", "Real_Housing_Index")

year_data$year <- as.numeric(year_data$year)
year_data <- subset(year_data, year >= 1995 & year <= 2025)

print(colnames(year_data))

print(
  ggplot(year_data, aes(x = year, y = Real_Housing_Index)) +
    geom_line(color = "dodgerblue") +
    geom_point(color = "dodgerblue") +
    labs(
      x = "Year",
      y = "Real Housing Price Index",
      title = "U.S. Real Housing Price Index (1995–2025)"
    ) +
    scale_x_continuous(breaks = seq(1995, 2025, by = 5)) +
    theme_minimal()
)

year_data

if (!require(knitr)) install.packages("knitr")
library(knitr)

year_data$Real_Housing_Index <- round(year_data$Real_Housing_Index, 1)

knitr::kable(
  year_data,
  col.names = c("Year", "Real Housing Price Index"),
  caption = "Average Annual Real Housing Price Index (1995–2025)"
)