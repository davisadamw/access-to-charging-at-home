library(tidyverse)

parking_with_vehs  <- read_rds('Data/parking_summary_2019.rds')

# what's the share of people who have off-street parking who see a possibility 
parking_with_vehs %>%  
  filter(!str_detect(source, 'ZEV')) %>% 
  filter(ev_cat %in% c('ICEV', 'Hybrid')) %>% 
  group_by(region) %>% 
  summarize(n  = n(),
            garage       = mean(any_charging & (parking_ranking <= 2)),
            carport      = mean(any_charging & (parking_ranking <= 3)),
            lot_driveway = mean(any_charging & (parking_ranking <= 4)))

