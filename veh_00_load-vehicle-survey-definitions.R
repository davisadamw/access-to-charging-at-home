library(tidyverse)
library(readxl)

definitions_file <- 'Data/tsdc-2019-california-vehicle-survey/documentation/tsdc-2019-california-vehicle-survey-data-dictionary.xlsx'

# sheet names don't quite match dataset names 
# ... need to replace 'CVS' with 'survey' 
# ... replace the one case where 'Veh' is used instead of 'vehicle'
# ... make lower case
fix_names <- function(name) {
  str_replace(name, 'CVS', 'survey') %>% 
    str_replace('Veh$', 'vehicle') %>% 
    str_to_lower() 
}

# load the relevant definition sheets
read_definitions <- function(sheet_name, table_name) {
  read_excel(definitions_file,
             sheet_name,
             col_names = c('var_name', 'var_label', 'value', 'value_label'),
             col_types = 'text',
             na = c('', 'N/A'),
             skip = 1) %>% 
    mutate(source = table_name) %>% 
    select(source, everything())
}

# first, load the sheets and attach them to names that match the csv files
# ... only want the residential survey component
veh_survey_definitions <- excel_sheets(definitions_file) %>%
  set_names(fix_names(.)) %>% 
  # use imap_dfr here because imap will pass the list element name (which matches the csv) in as the second argument
  # there's not a particularly good reason to do it like this instead of within read_definitions
  imap_dfr(read_definitions) %>% 
  # then remove the survey of commercial vehicles 
  filter(str_detect(source, 'res'))

# not all of the entries in this table are actually value maps
# some of them have only a single row (providing a unit) 
# additionally, the excel file lists var_name and label with the first row of definitions, then is blank below that
# to replicate variable names and labels down to all row, group by counts of non-na's
veh_survey_definitions_cl <- veh_survey_definitions %>% 
  # they were all integer stored as text, so this is fine
  mutate(value = as.integer(value)) %>%
  drop_na(value) %>% 
  group_by(var_grp = cumsum(!is.na(var_name))) %>% 
  mutate(var_name  = first(var_name),
         var_label = first(var_label),
         n_vals    = n()) %>% 
  ungroup() %>%  
  select(source:var_label,
         value, value_label)

# some variables appear in multiple tables ... lets see if their values are coded consistently across the tables
veh_survey_definitions_duplicates <- veh_survey_definitions_cl %>% 
  select(-var_label) %>% 
  distinct(var_name, value, value_label) %>% 
  group_by(var_name, value) %>% 
  # then therea aren't any left. Good!
  filter(n() > 1)

# this means, we can just force case and collapse accross all the tables
veh_survey_definitions_cl_nodupes <- veh_survey_definitions_cl %>%
  select(-source) %>% 
  distinct(var_name, value, value_label)

# write the result to disk
veh_survey_definitions_cl %>% 
  write_rds('Data/ca_veh_survey_defs_2019.rds') %>% 
  write_csv('Data/ca_veh_survey_defs_2019.csv')





