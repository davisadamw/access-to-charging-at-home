library(tidyverse)
library(vroom)

# the data dictionary to the data dictionary file is in the pdf in raw_data
pums_dd <- read_csv('Raw_Data/PUMS/PUMS_Data_Dictionary_2014-2018.csv', 
                    col_names = c('identifying_flag', 'pums_variable',
                                  'variable_type', 'length', 
                                  'starting_value', 'ending_value',
                                  'description'))
pums_hh <- vroom('Raw_Data/PUMS/psam_h06.csv')

# remove variables with no variation
dropper_vals <- pums_hh %>% 
  map_int(~ length(unique(.))) %>% 
  enframe(name = 'pums_variable',
          value = 'uniques') %>% 
  filter(uniques == 1)

# subset the data dictionary entries to keep only those for variables appearing with variation in pums_hh
pums_dd4hh <- pums_dd %>%
  filter(pums_variable %in% names(pums_hh),
         !pums_variable %in% dropper_vals$pums_variable)

# separate variable names from variable values
pums_dd4hh_names <- pums_dd4hh %>% 
  filter(identifying_flag == 'NAME') %>% 
  select(pums_variable:length,
         description = starting_value)

pums_dd4hh_names %>% clipr::write_clip()

# finally, after inspection, let's grab the variables we want
# first, replicate weights for estimating variance
pums_hh_wt <- pums_hh %>% 
  select(matches('WGTP[0-9]+$'))

pums_hh_keep <- pums_hh %>% 
  select(SERIALNO, PUMA, WGTP, NP, TYPE, ACR, BLD, RMSP, 
         TEN, VACS, VALP, VEH, YBL, FES, HUPAC, SRNT, SVAL, 
         FACRP, FBLDP, FMVP, FRMSP, FTENP, FVACSP, FVALP, FVEHP)

# and get the value keys for these variables
pums_dd4hh_values <- pums_dd4hh %>% 
  filter(identifying_flag == 'VAL',
         pums_variable %in% names(pums_hh_keep)) %>% 
  select(pums_variable, starting_value:description)

# value map will be applied only when starting_value and ending_value are same
# to do this, convert it into a nested list 
# ... list will have elements for columns in pums_hh, each one a named vector of potential values
pums_dd4hh_values_4map <- pums_dd4hh_values %>% 
  filter(starting_value == ending_value, 
         pums_variable != 'WGTP') %>% 
  select(pums_variable, value = starting_value, text = description) %>% 
  group_by(pums_variable) %>% 
  nest(val_map_tbl = c(value, text)) %>% 
  mutate(val_map = map(val_map_tbl, deframe)) %>% 
  select(-val_map_tbl) %>% 
  deframe()

apply_names <- function(values, col_name, val_maps = pums_dd4hh_values_4map) {
  # grab relevant values map
  val_map <- pluck(val_maps, col_name)

  # there is no value map for that column, return the column as is
  if (is.null(val_map)) return(values)
  
  # otherwise, use the starting values as index to pull from value map
  
  # if val_map is a named vector, use those
  if (!is.null(names(val_map))) return(unname(val_map[as.character(values)]))
  
  # otherwise, assume values is usable as an index
  unname(val_map[values])
}

# apply names map ... imap will iterate over columns, giving vector as first arg, col name as second arg
pums_hh_keep_named <- pums_hh_keep %>% 
  imap(apply_names) %>% 
  as_tibble()

# finally replace the names with something more descriptive
# basically just snake case of the first few words (letters separated by spaces or slashes)
pums_dd4hh_names_use <- pums_dd4hh_names %>% 
  mutate(name_use = 
           snakecase::to_snake_case(
             str_extract(description, '[A-Za-z /]+'))
  ) %>% 
  select(pums_variable, name_use) %>% 
  deframe()

pums_hh_keep_renamed <- pums_hh_keep_named %>% 
  rename_all(~ ifelse(. %in% c('SERIALNO', 'PUMA'), ., pums_dd4hh_names_use[.]))

# save the data you need plus the weights
pums_hh_keep_renamed %>% 
  write_rds('Data/ca_hh_housinvars.rds', compress = 'gz')
