source(here("R","raw","raw-utils.R"))
library(googledrive)

import_canpl <- function(save_folder=here("data","CPL")){
  
  find_cc <- drive_find(pattern="Centre Circle") #auto select account?
  get_cc <- drive_get(as_id(find_cc[1,]))
  
  csv_files <- drive_ls(get_cc, type = "csv") %>%
    mutate(path=glue({"{save_folder}/{name}"}))
  
  for(i in 1:dim(csv_files)[1]){
    drive_download(file=csv_files[i,],path=as.character(csv_files[i,"path"]),overwrite=TRUE)
  }
}

# csv_files %>%
#   pwalk(list(.$id),~drive_download(as_id(.x)))

# walk(csv_files$id, ~ drive_download(as_id(.x),path=glue("{path}/{csv_files$id}"),overwrite=TRUE))
