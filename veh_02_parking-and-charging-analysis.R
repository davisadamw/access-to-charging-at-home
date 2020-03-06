library(tidyverse)

parking_with_vehs  <- read_rds('Data/parking_summary_2019.rds') %>% 
  filter(!str_detect(source, 'ZEV'))

# what's the share of people who have off-street parking who see a possibility 
parking_with_vehs %>%  
  filter(ev_cat %in% c('ICEV', 'Hybrid')) %>% 
  group_by(region) %>% 
  summarize(n  = n(),
            garage       = mean(any_charging & (parking_ranking <= 2)),
            carport      = mean(any_charging & (parking_ranking <= 3)),
            lot_driveway = mean(any_charging & (parking_ranking <= 4)))

# hmm... what about looking at housing x parking status
parking_with_vehs %>% 
  replace_na(list(parking_level = 'NA_parking')) %>%
  count(housing, parking_level, sort = T) %>% 
  pivot_wider(names_from = parking_level, 
              values_from = n, 
              values_fill = list(n = 0)) %>% 
  mutate(tot = rowSums(.[,-1])) %>% 
  bind_rows(summarize_at(., vars(-housing), sum))

# measured literally just off the figure for San Diego (224 units for 80%)
kurani_l2 <- tribble(
  ~variable,  ~value,      ~total, ~l1,  ~l2,
  'house_ak', 'detached',   201,   164,  84,
  'house_ak', 'attached',    42,    23,   9,
  'house_ak', 'apartment',   25,     6,   2,
  'house_ak', 'mobile',       9,     7,   2,
  'parki_ak', 'garage',     126,   107,  56,
  'parki_ak', 'driveway',    91,    68,  28,
  'parki_ak', 'carport',     20,    11,   4,
  'parki_ak', 'street',      27,    13,   6,
  'parki_ak', 'lot',         15,     3,   2
) %>% 
  mutate_at(vars(total:l2), ~ . / 224 * 0.8) %>% 
  mutate(l1_t = l1 / total,
         l2_t = l2 / total) %>% 
  select(variable, value, ends_with('_t'))

parking_with_vehs %>% 
  select(housing, parking_level) %>% 
  mutate(parki_ak = if_else(str_starts(parking_level, 'garage'), 'garage', parking_level),
         house_ak = 
           case_when(
             str_starts(housing, 'Single family house not') ~ 'detached',
             str_starts(housing, 'Single family house att') ~ 'attached',
             str_detect(housing, 'mobile')                  ~ 'mobile',
             TRUE                                           ~ 'mobile')
  ) %>% 
  left_join(select(kurani_l2, value, l1_parking = l1_t, l2_parking = l2_t), by = c('parki_ak' = 'value')) %>% 
  left_join(select(kurani_l2, value, l1_housing = l1_t, l2_housing = l2_t), by = c('house_ak' = 'value')) %>% 
  summarise_at(vars(starts_with('l')), mean, na.rm = TRUE)
  

                     
# okay, so first... using Kurani's outputs
parking_with_vehs %>% 
  count(housing) %>% 
  mutate(wt_Kurani = 
           case_when(
             ))
