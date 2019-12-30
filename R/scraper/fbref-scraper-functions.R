fbref_scrape <- function(fbref_url,nodes=NA,fix_columns=FALSE,extract=NA){
  
  data_table <-
    read_html(fbref_url) %>%
    html_nodes("table") %>%
    html_table
  
  if(is.na(extract)==FALSE){
    data_table <- data_table %>%
      extract2(extract)
  }
  
  if(fix_columns==TRUE){
    names(data_table) <- data_table[1,] # move first row to names
    data_table <- as_tibble(data_table, .name_repair="unique") # create tibble
    data_table <- data_table %>% slice(-1) # remove first row
    data_table <- type_convert(data_table) # reset types
  }
  else {
    data_table <- as_tibble(data_table, .name_repair = "unique") # create tibble
  }
  
  return(data_table)
}

#function to import from local CSV file
fbref_import <- function(file,skip=0){
  path <- "./data/fbref/EPL/2019/"
  
  data_csv <- read_csv(paste0(path,file),skip=skip)
  
  return(data_csv)
}


#function to save data as RDS
save_data <- function(data){
  saveRDS(
    object=get(data),
    file=paste0("./data/",data,".rds")
  )
}