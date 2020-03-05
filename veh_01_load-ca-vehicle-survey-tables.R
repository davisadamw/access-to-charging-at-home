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

# what percent of households have 2 commutes >= 30 miles one way?
ca_veh_survey$person %>% 
  filter(work_mode == 'Drive alone using household vehicle') %>% 
  mutate(commute_30plus = work_distance >= 30) %>% 
  group_by(sampno) %>% 
  summarise(n_tot = n(),
            n_30  = sum(commute_30plus)) %>% 
  count(n_30 > 1)

# grab a little info about vehicles 
vehicles_fewvars <- ca_veh_survey$vehicle %>% 
  # need to get the ev category variable down into one column
  mutate(ev_cat = case_when(flag_hev  == 'Selected' ~ 'Hybrid',
                            flag_phev == 'Selected' ~ 'PHEV',
                            flag_fcev == 'Selected' ~ 'FCEV',
                            flag_bev  == 'Selected' ~ 'BEV',
                            TRUE      ~ 'ICEV')) %>% 
  select(sampno:vehicle_num, year, make_clean, fuel_clean, 
         veh_type_clean, ev_cat, annual_mileage, mpg,
         current_primary_driver, primary_driver_id) 

# parking info is a multi-option thing stored in the definitions file
parking_descs <- veh_survey_definitions %>% 
  filter(str_starts(var_name, 'home_parking')) %>% 
  distinct(var_name, var_label) %>% 
  mutate(parking_type = str_extract(var_label, '(?<=\\?: )[A-Za-z ]+') %>% 
           snakecase::to_snake_case()) %>% 
  select(var_name, parking_type) %>% 
  deframe()

rename_parking <- function(name) parking_descs[name]

parking_ranking <- tribble(
  ~parking_type,                             ~parking_level,    ~parking_ranking,
  'driveway',                                'driveway',        4,
  'on_the_street',                           'street',          9,
  'attached_garage',                         'garage_attached', 1,
  'detached_garage',                         'garage_detached', 2,
  'unassigned_parking_in_lot_or_garage',     'lot',             3,
  'none_of_these_are_available_at_my_home',  'street',          9,
  'assigned_parking_in_lot_or_garage',       'lot',             4,
  'carport',                                 'carport',         3,
  'other',                                   'street',          9
)

# only keep the "best" parking situation per household
parking_status <- ca_veh_survey$main %>% 
  select(sampno, starts_with('home_parking'), -home_parking_8_x) %>% 
  pivot_longer(-sampno, names_to = 'var_name', values_to = 'present') %>% 
  filter(present == 'Selected') %>% 
  left_join(select(veh_survey_definitions, var_name, var_label), 
            by = 'var_name') %>% 
  mutate(parking_type = str_extract(var_label, '(?<=\\?: )[A-Za-z ]+') %>% 
           snakecase::to_snake_case()) %>% 
  left_join(parking_ranking, by = 'parking_type') %>% 
  group_by(sampno) %>% 
  filter(parking_ranking == min(parking_ranking)) %>% 
  ungroup() %>% 
  distinct(sampno, parking_level)
  
    

# parking info ... multi choice stored awkwardly, need to rename variables
# ... also attach vehicle info and drop households without any vehicles
parking_with_vehs <- ca_veh_survey$main %>% 
  select(sampno, source, region,
         home_electricity_access) %>% 
  left_join(parking_status, by = 'sampno') %>% 
  inner_join(vehicles_fewvars, by = 'sampno')

parking_with_vehs %>% 
  write_rds('Data/parking_summary_2019.rds')

