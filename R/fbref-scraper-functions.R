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

#function to save data as RDS
save_data <- function(data){
  saveRDS(
    object=get(data),
    file=paste0("./data/",data,".rds")
  )
}

#function to read data from RDS
# read_data <- function(){
#   # readRDS(
#   #   file=paste0("./data/",
#   #               list.files(path="./data",pattern="_raw.rds"))
#   # )
#   
#   # filepath <- "./data/"
# 
#   # matches_raw <- readRDS(paste0(filepath,"matches_raw.rds"))
#   # squad_keepers_raw <- readRDS(paste0(filepath,"squad_keepers_raw.rds"))
#   # squad_misc_raw <- readRDS(paste0(filepath,"squad_misc_raw.rds"))
#   # squad_passing_raw <- readRDS(paste0(filepath,"squad_passing_raw.rds"))
#   # squad_playingtime_raw <- readRDS(paste0(filepath,"squad_playingtime_raw.rds"))
#   # squad_shooting_raw <- readRDS(paste0(filepath,"squad_shooting_raw.rds"))
#   # squad_standard_raw <- readRDS(paste0(filepath,"squad_standard_raw.rds"))
#   # table_raw <- readRDS(paste0(filepath,"table_raw.rds"))
# }

# readRDS(paste0("./data/","matches_raw.rds"))
# readRDS(paste0("./data/",
#                list.files(path="./data",pattern="_raw.rds")))

# list.files(path="./data",pattern="_raw.rds")
# paste0("./data/",
#        list.files(path="./data",pattern="_raw.rds"))

# https://stats.idre.ucla.edu/r/codefragments/read_multiple/