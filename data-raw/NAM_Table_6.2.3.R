## New Try Table 6.2.3

# https://www.ssb.no/en/statbank/table/08792/tableViewLayout1/
# 08792: External trade in goods, main figures (NOK million), by trade flow, month and contents
# Exact Query: https://www.ssb.no/en/statbank/sq/10061468
library(tidyverse)
library(here)


ships <- read_delim(here("08792_20220103-162301.csv"), skip = 1, delim = ";", na = "-")
ships %>% 
  rename(ex_ships_oilp = Value) %>% 
  select(-`trade flow`) %>% 
  replace_na(list(ex_ships_oilp = 0)) %>% 
  separate(month, sep = "M", into = c("year","month")) %>% 
  mutate(month = as.numeric(month), 
         date = as.Date(paste0(year,"-",month,"-01")), 
         quarter = lubridate::quarter(date)) %>% 
  group_by(year,quarter) %>%
  summarise(ex_ships_oilp = sum(ex_ships_oilp),.groups = "drop") -> ships_q


# to get the sample right - in the final quarter there is one month missing
# we can either drop the final observation or (here) just aggregate over the two remaining months
ships_q %>% 
  filter(year < 2015) %>% 
  slice(1:138) -> ships_q_ready

ships_q -> ships_q_ready

ships_q_ready %>% 
  mutate(DLOG_ASKIP = diff(c(NA,log(ex_ships_oilp))), 
         LOG_ASKIP_1 = log(lag(ex_ships_oilp)),
         c1 = ifelse(quarter == 1,0.75,-0.25),
         c2 = ifelse(quarter == 2,0.75,-0.25),
         c3 = ifelse(quarter == 3,0.75,-0.25)) %>% #summarise(mean(DLOG_ASKIP, na.rm = TRUE))
  
  lm(DLOG_ASKIP ~ LOG_ASKIP_1 + c1 + c2 + c3, data = ., ) %>% summary

