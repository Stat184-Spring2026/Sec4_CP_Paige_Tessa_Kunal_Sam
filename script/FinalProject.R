# Load necessary libraries for reading data and plotting
library(readr)
library(ggplot2)

# Read in the housing price index dataset
QUSR628BIS <- read_csv("QUSR628BIS.csv", show_col_types = FALSE)

# Create yearly averages of the housing price index
year_data <- aggregate(
  QUSR628BIS$QUSR628BIS,
  by = list(year = format(QUSR628BIS$observation_date, "%Y")),
  FUN = mean
)

# Rename columns to be more readable
colnames(year_data) <- c("year", "Real_Housing_Index")

# Convert year from character to numeric so it can be plotted correctly
year_data$year <- as.numeric(year_data$year)
# Keep only data between 1995 and 2025
year_data <- subset(year_data, year >= 1995 & year <= 2025)

# Check column names (helps confirm data is structured correctly)
print(colnames(year_data))

# Create a line graph showing housing price trends over time
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

# Print the processed dataset to view values
year_data

# Install and load knitr package (used for creating tables)
if (!require(knitr)) install.packages("knitr")
library(knitr)

# Round the housing index values to 1 decimal place for readability
year_data$Real_Housing_Index <- round(year_data$Real_Housing_Index, 1)

# Create a formatted table of the yearly data
knitr::kable(
  year_data,
  col.names = c("Year", "Real Housing Price Index"),
  caption = "Average Annual Real Housing Price Index (1995–2025)"
)