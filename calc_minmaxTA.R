library(tidyverse)
library(lubridate)


#read in hourly data in order to calculate daily min/max Ta, then average per week/month


hourly_Ta_SE_Htm <- read_csv("R:/DATA/ICOS_warm_winter/FLX_SE-Htm_FLUXNET2015_FULLSET_2015-2020_beta-3/FLX_SE-Htm_FLUXNET2015_FULLSET_HH_2015-2020_beta-3.csv", 
                             col_types = cols(TIMESTAMP_START = col_datetime(format = "%Y%m%d%H%M")), col_names=TRUE, col_select = c(TIMESTAMP_START, TA_F))
                             

# get daily min and max temps

daily_min_max_SE_Htm <- hourly_Ta_SE_Htm %>%
  group_by(date = floor_date(TIMESTAMP_START, "day")) %>%
  summarize(min_Ta = min(TA_F), max_Ta = max(TA_F))

# get monthly mean and max daily temps

monthly_min_max_SE_Htm <- daily_min_max_SE_Htm %>%
  group_by(month = floor_date(date, "month")) %>%
  summarize(monthly_mean_minTa = mean(min_Ta), monthly_mean_maxTa = mean(max_Ta))

# filter desired date range
monthly_min_max_SE_Htm_18_20 <- monthly_min_max_SE_Htm %>%
  filter(month >= "2018-01-01")

# copy to clipboard
writeClipboard(as.character(monthly_min_max_SE_Htm_18_20$monthly_mean_maxTa))
