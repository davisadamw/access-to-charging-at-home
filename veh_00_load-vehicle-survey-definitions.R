library(tidyverse)
library(readxl)

# load the relevant definition sheets
read_definitions <- function(sheet_name) {
  read_excel('Data/ca_veh_surv_res/cec_data_definitions_fixedsheetname.xlsx',
             sheet_name,
             col_names = c('var_name', 'var_label', 'value', 'value_label'),
             col_types = 'text',
             skip = 1) %>% 
    mutate(source = sheet_name) %>% 
    select(source, everything())
}

# this excel file contains a sheet for each csv in the original dataset
# load the sheets with definitions for the files I want to use
veh_survey_definitions <- list.files('Data/ca_veh_surv_res', 'survey_res_.*.csv') %>%
  str_remove('.csv') %>% 
  set_names(.) %>% 
  map_dfr(read_definitions)

# not all of the entries in this table are actually value maps
# some of them have only a single row (providing a unit) or a partial / non-numeric value
# usable value maps here ONLY have integers in the value column
# additionally, the excel file lists var_name and label with the first row of definitions, then is blank below that
# to replicate variable names and labels down to all row, group by counts of non-na's
veh_survey_definitions_cl <- veh_survey_definitions %>% 
  mutate(value_int = as.integer(str_extract(value, '^[0-9]+$'))) %>% 
  group_by(var_grp = cumsum(!is.na(var_name))) %>% 
  mutate(var_name  = first(var_name),
         var_label = first(var_label),
         all_ints  = all(!is.na(value_int)),
         n_vals    = n()) %>% 
  ungroup() %>% 
  filter(all_ints) %>% 
  select(source:var_label,
         value = value_int,
         value_label)

veh_survey_definitions_cl %>% 
  write_rds('Data/ca_veh_survey_defs.rds') %>% 
  write_csv('Data/ca_veh_survey_defs.csv')





