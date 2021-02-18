library(tidyverse)
url="https://www.ncdc.noaa.gov/teleconnections/pdo/data.csv"
download.file(url, "pdo.csv")
pdo_dat <- read_csv("pdo.csv", skip=1)
colnames(pdo_dat) <- c("date", "pdo")

## separate `date` into `year` & `month`
pdo_mon <- pdo_dat %>%
  select(date, pdo) %>%
  mutate(year = as.integer(sub("(^[0-9]{4})([0-9]{2})", "\\1", date)),
         month = as.integer(sub("(^[0-9]{4})([0-9]{2})", "\\2", date)),
         ## assign summer & winter months to `period`
         period = ifelse(month >= 4 & month <= 9, "summer", "winter"),
         date = NULL) %>%
  select(year, month, period, pdo)

## create period means by year
pdo_yr <- pdo_mon %>%
  mutate(brood_year = year + 2) %>%
  group_by(brood_year, period) %>%
  summarise(pdo = mean(pdo)) %>%
  pivot_wider(names_from = period, values_from = pdo)