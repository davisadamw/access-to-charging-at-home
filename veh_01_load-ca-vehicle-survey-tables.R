library(tidyverse)
library(definer)

# load definitions
veh_survey_definitions <- read_rds('Data/ca_veh_survey_defs_2019.rds')

veh_survey_dict <- veh_survey_definitions %>% 
  def_prep(var_name, value, value_label)

# load the survey files we care about and apply the definitions
# ... some of the columns have weird values lower down the rows, so guess/sniff based on all rows
ca_veh_survey <- list.files('Data/tsdc-2019-california-vehicle-survey/data',
                            'survey_res*',
                            recursive = TRUE, full.names = TRUE) %>% 
  set_names(str_extract(., '[a-z]+(?=\\.csv)')) %>% 
  map(read_csv, guess_max = 40000) %>% 
  map(def_recode_all, def_list = veh_survey_dict, .default = 'unspecified')


# grab a little info about vehicles 
vehicles_fewvars <- ca_veh_survey$vehicle %>% 
  select(sampno:vehicle_num, year, make_clean, fuel_clean, 
         veh_type_clean, flag_hev:flag_bev, annual_mileage, mpg,
         current_primary_driver, primary_driver_id) %>% 
  # need to get the ev category variable down into one column
  #mutate(ev_cat = )

# parking info is a multi-option thing stored in the definitions file
parking_descs <- veh_survey_definitions %>% 
  filter(str_starts(var_name, 'home_parking')) %>% 
  distinct(var_name, var_label) %>% 
  mutate(parking_type = str_extract(var_label, '(?<=\\?: )[A-Za-z ]+') %>% 
           snakecase::to_snake_case()) %>% 
  select(var_name, parking_type) %>% 
  deframe()

rename_parking <- function(name) parking_descs[name]

# parking info ... multi choice stored awkwardly, need to rename variables
# ... also attach vehicle info and drop households without any vehicles
parking_with_vehs <- ca_veh_survey$main %>% 
  select(sampno, source, california, region, county, 
         home_electricity_access, 
         starts_with('home_parking'), -home_parking_8_x) %>% 
  rename_at(vars(starts_with('home_parking')), rename_parking) %>% 
  inner_join(vehicles_fewvars, by = 'sampno')

# basic parking info
parking_with_vehs <- parking_with_vehs %>% 
  mutate(charging_case = 
           case_when(str_starts(housing, 'Single family house not attached') &
                       parkingtype == 'Personal garage' ~ 'highest',
                     str_starts(housing, 'Single family house') &
                       str_starts(parkingtype, 'Personal') ~ 'high',
                     TRUE ~ 'low'))

parking_with_vehs %>% 
  count(vehtype, vehfueltype, charging_case) %>% 
  pivot_wider(names_from = charging_case, values_from = n, values_fill = list(n = 0)) %>% 
  mutate(total = rowSums(.[, c('high', 'highest', 'low')])) %>% 
  mutate_at(vars(high, highest, low), ~ . / total) %>% 
  View()


