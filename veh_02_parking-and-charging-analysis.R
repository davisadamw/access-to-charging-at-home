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
axsen_kurani <- tribble(
  ~variable,  ~value,      ~total, ~l1, ~l2,
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

parking_with_vehs_ak <- parking_with_vehs %>% 
  select(housing, parking_level, veh_type_clean, any_charging, parking_ranking) %>% 
  mutate(
    parki_ak = if_else(str_starts(parking_level, 'garage'), 'garage', parking_level),
    house_ak = 
      case_when(
        str_starts(housing, 'Single family house not') ~ 'detached',
        str_starts(housing, 'Single family house att') ~ 'attached',
        str_detect(housing, 'mobile')                  ~ 'mobile',
        TRUE                                           ~ 'mobile')
  ) %>% 
  separate(veh_type_clean, 
           c('veh_size', 'veh_body'),
           sep = ' ',
           extra = 'merge') %>% 
  left_join(select(axsen_kurani, value, l1_parking = l1_t, l2_parking = l2_t), by = c('parki_ak' = 'value')) %>% 
  left_join(select(axsen_kurani, value, l1_housing = l1_t, l2_housing = l2_t), by = c('house_ak' = 'value')) %>% 
  mutate(l1_avg = (l1_parking + l1_housing) / 2,
         l2_avg = (l2_parking + l2_housing) / 2,
         lX_gar = any_charging & (parking_ranking <= 2),
         lX_lot = any_charging & (parking_ranking <= 4))
  

# housing/parking summary overall
parking_with_vehs_ak %>% 
  summarise_at(vars(starts_with('l')), mean, na.rm = TRUE)

# broken down by vehicle type ... not much here
parking_with_vehs_ak %>% 
  group_by(veh_size, veh_body) %>% 
  group_by(n = n(), add = TRUE) %>% 
  summarise_at(vars(starts_with('l')), mean, na.rm = TRUE) %>% 
  select(starts_with('veh'), n, starts_with('lX')) %>% 
  arrange(desc(lX_gar)) %>% 
  mutate_at(vars(starts_with('lX')), scales::percent_format(0.1)) %>% clipr::write_clip()
  

