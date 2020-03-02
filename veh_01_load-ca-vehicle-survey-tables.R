library(tidyverse)

ca_veh_survey <- list.files('Data/ca_veh_surv_res',
                            recursive = TRUE, full.names = TRUE) %>% 
  set_names(str_extract(., '[a-z]+(?=\\.csv)')) %>% 
  map(read_csv)

