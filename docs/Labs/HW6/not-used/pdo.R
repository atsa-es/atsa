library(tidyverse)
# Something wrong with this
url="https://www.ncdc.noaa.gov/teleconnections/pdo/data.csv"
download.file(url, "pdo.csv")
pdo_dat <- read_csv("pdo.csv", skip=1)
colnames(pdo_dat) <- c("date", "pdo")

## separate `date` into `year` & `month`
pdo_mon <- pdo_dat %>%
  select(date, pdo) %>%
  mutate(year = as.integer(sub("(^[0-9]{4})([0-9]{2})", "\\1", date)),
         month = as.integer(sub("(^[0-9]{4})([0-9]{2})", "\\2", date)),
         period = ifelse(month >= 3 & month <= 9, "summer", "winter"),
         date = NULL) %>%
  select(year, month, period, pdo)
pdo_mon$year[pdo_mon$month %in% 10:12] <- pdo_mon$year[pdo_mon$month %in% 10:12] + 1

## create period means by year
pdo_yr <- pdo_mon %>%
  group_by(year, period) %>%
  summarise(pdo = mean(pdo)) %>%
  pivot_wider(names_from = period, values_from = pdo)

save("pdo_yr", file="pdo.RData")