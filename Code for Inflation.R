library(readr)
library(ggplot2)
library(dplyr)
library(knitr)

inflation <- read_csv("FPCPITOTLZGUSA.csv", show_col_types = FALSE)
colnames(inflation)
head(inflation)

inflation <- inflation %>%
  rename(
    year = observation_date,
    Inflation = FPCPITOTLZGUSA
  )

inflation$year <- as.numeric(format(as.Date(inflation$year), "%Y"))

inflation <- inflation %>%
  filter(year >= 1995 & year <= 2025)
print(head(inflation))

ggplot(inflation, aes(x = year, y = Inflation)) +
  geom_line(color = "dodgerblue") +
  geom_point(color = "dodgerblue") +
  geom_hline(
    yintercept = mean(inflation$Inflation, na.rm = TRUE),
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  labs(
    x = "Year",
    y = "Inflation Rate (%)",
    title = "U.S. Inflation Rate (1995–2025)"
  ) +
  scale_x_continuous(breaks = seq(1995, 2025, by = 5)) +
  theme_minimal()

inflation$Inflation <- round(inflation$Inflation, 2)

kable(
  inflation,
  col.names = c("Year", "Inflation Rate (%)"),
  caption = "Annual U.S. Inflation Rate (1995–2025)"
)