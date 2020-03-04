library(tidyverse)
library(definer)

# load definitions
veh_survey_definitions <- read_rds('Data/ca_veh_survey_defs.rds')

veh_survey_dict <- veh_survey_definitions %>% 
  def_prep(var_name, value, value_label)

# load the survey files we care about and apply the definitions
ca_veh_survey <- list.files('Data/ca_veh_surv_res', 'survey_res*',
                            recursive = TRUE, full.names = TRUE) %>% 
  set_names(str_extract(., '[a-z]+(?=\\.csv)')) %>% 
  map(read_csv) %>% 
  map(def_recode_all, def_list = veh_survey_dict, .default = 'unspecified')


# parking info
parking_with_vehs <- ca_veh_survey$main %>% 
  select(sampno, county, housing, parkingtype, parkingpay) %>% 
  left_join(select(ca_veh_survey$currentvehs, sampno:vehmake, vehfueltype), by = 'sampno')

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


