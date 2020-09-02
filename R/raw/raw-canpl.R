import_canpl <- function(save_folder=here("data","CPL"),save_path=here("data","canpl-raw.rds")){

  find_cc <- drive_find(pattern="Centre Circle Data & Info") #auto select account?
  get_cc <- drive_get(as_id(find_cc[1,]))
  
  csv_files <- drive_ls(get_cc, type = "csv") %>%
    mutate(path=glue({"{save_folder}/{name}"}))
  
  for(i in 1:dim(csv_files)[1]){
    drive_download(file=csv_files[i,],path=as.character(csv_files[i,"path"]),overwrite=TRUE)
  }
  
  csv_local <- dir(path=save_folder,pattern=".csv")
  
  canpl <- tibble(name=csv_local) %>%
    mutate(path=glue("{save_folder}/{csv_local}")) %>%
    mutate(data=map(path,read_csv))
  
  saveRDS(canpl,file=save_path)
  
  return(canpl)
}

# csv_files %>%
#   pwalk(list(.$id),~drive_download(as_id(.x)))

# walk(csv_files$id, ~ drive_download(as_id(.x),path=glue("{path}/{csv_files$id}"),overwrite=TRUE))
