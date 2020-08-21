source(here("R","raw","raw-utils.R"))
library(googlesheets4)

import_canpl <- function(save_path=here("data","canpl-raw.rds")){
  
  canpl <- tribble(~season, ~table, ~url,
                   "2020","Team Totals","1QaFAXjW6O68gpMFLRIBVf-EdIYeKznqvNQQy6cc5DNM",
  )
  
  canpl %<>%
    mutate(data=map(url,possibly(googlesheets4::read_sheet,otherwise=NA)))
  
}

canpl <- import_canpl()
