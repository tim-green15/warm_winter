library(tidyverse)
library(lubridate)


#read in hourly data in order to calculate daily min/max Ta, then average per week/month


hourly_Ta_SE_Htm <- read_csv("R:/DATA/ICOS_warm_winter/FLX_SE-Htm_FLUXNET2015_FULLSET_2015-2020_beta-3/FLX_SE-Htm_FLUXNET2015_FULLSET_HH_2015-2020_beta-3.csv", 
                             col_types = cols(TIMESTAMP_START = col_datetime(format = "%Y%m%d%H%M")), col_names=TRUE, col_select = c(TIMESTAMP_START, TA_F))
                             

# get daily min and max temps. in weekly I filtered first to ensure the 7 day grouping begins on the 01-01 of desired year. also created levels associated with year
# (letters A-C) and weeks (1-52). see scrap at bottom 

daily_min_max_SE_Htm <- hourly_Ta_SE_Htm %>%
  group_by(date = floor_date(TIMESTAMP_START, "day")) %>%
  summarize(min_Ta = min(TA_F), max_Ta = max(TA_F))%>%
  mutate(woy = week(date)) 

daily_min_max_SE_Htm_18_20 <- hourly_Ta_SE_Htm %>%
  group_by(date = floor_date(TIMESTAMP_START, "day")) %>%
  summarize(min_Ta = min(TA_F), max_Ta = max(TA_F), mean_Ta = mean(TA_F))%>%
  filter(date >= "2018-01-01") %>%
  mutate(week_ABC = factor(woy_htm_52_ABC, levels=levels_weeks))


# get monthly/weekly mean and max daily temps

monthly_min_max_SE_Htm <- daily_min_max_SE_Htm %>%
  group_by(month = floor_date(date, "month")) %>%
  summarize(monthly_mean_minTa = mean(min_Ta), monthly_mean_maxTa = mean(max_Ta))

weekly_min_max_SE_Htm <- daily_min_max_SE_Htm_18_20 %>%
  group_by(week_ABC) %>%
  summarize(weekly_mean_minTa = mean(min_Ta), weekly_mean_maxTa = mean(max_Ta), weekly_meanTA = mean(mean_Ta))

# filter desired date range
monthly_min_max_SE_Htm_18_20 <- monthly_min_max_SE_Htm %>%
  filter(month >= "2018-01-01")

weekly_min_max_SE_Htm_18_20 <- weekly_min_max_SE_Htm %>%
  filter(week >= "2018-01-01")

# copy to clipboard
writeClipboard(as.character(weekly_min_max_SE_Htm$weekly_mean_maxTa))


###### week of year scrap

woy_Htm <- daily_min_max_SE_Htm$woy
woy_htm_52 <- ifelse(woy_Htm == 53, 52, woy_Htm)

# must rep 365/366 when adding factor label to daily. this was changed in re-order factor levels in order to match sequential dates, otherwise R orders in undesirable way
#eg A1, A10; rather than A1, A2
A <- rep("A", 52)
B <- rep("B", 52)
C <- rep("C", 52)
yrs <- c(A, B, C)
nums <- rep(seq(1,52,1),3)

levels_weeks <- paste0(yrs,nums)


woy_htm_52_ABC <- paste0(yrs, woy_htm_52)

SE_Htm_18_20 <- SE_Htm %>%
  filter(TIMESTAMP >= "2018-01-01")
SE_Htm_18_20$TA_F
