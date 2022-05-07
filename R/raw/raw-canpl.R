import_canpl <- function(save_folder=here("data","CPL"),save_path=here("data","canpl.rds")){
  
  centre_circle_data_folders <-
    drive_ls(pattern="Centre Circle Data") %>%
    drive_ls() %>%
    filter(str_detect(name,"Season"))
  
  for(i in 1:dim(centre_circle_data_folders)[1]){
    csv_files <-
      centre_circle_data_folders %>%
      slice(i) %>%
      drive_ls(path=., type="csv") %>%
      mutate(path=glue("{save_folder}/{name}"))
    
    for(j in 1:dim(csv_files)[1]){
      drive_download(file=csv_files[j,],path=as.character(csv_files[j,"path"]),overwrite=TRUE)
    }
  }
  
  csv_local_files <- dir(path=save_folder,pattern=".csv")
  
  canpl <- tibble(name=csv_local_files) %>%
    mutate(data=map(glue("{save_folder}/{csv_local_files}"),read_csv))
  
  saveRDS(canpl,file=save_path)
  return(canpl)
}
