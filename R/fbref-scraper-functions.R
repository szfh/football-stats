# function to scrape fbref
fbref_scrape <- function(url,nodes,ncol=1,skip_head=0,skip_tail=0,fix_columns=FALSE,remove_last_row=FALSE){

  data_text <-
    url %>%
    read_html() %>% 
    html_nodes(nodes) %>% 
    html_text

  if(skip_head!=0){
    data_text <- data_text %>%
      enframe() %>%
      slice(-c(1:skip_head)) %>%
      deframe
  }
  
  if(skip_tail!=0){
    data_text <- data_text %>%
      enframe() %>%
      slice(-c(length(data_text)-skip_tail+1:length(data_text))) %>%
      deframe
  }
  
  data <- data_text %>%
    matrix(ncol=ncol,byrow=T) %>%
    as_tibble(.name_repair = "minimal")

  if(fix_columns==TRUE){
    data <- first_row_to_columns(data)
  }
  
  if(remove_last_row==TRUE){
    data <- remove_last_row(data)
  }

  return(data)
}

#function to make the first row of a tibble into unique column names, then delete the first row
first_row_to_columns <- function(data){
  data <- data %>%
    set_colnames(
      make.names(data[1,],unique=TRUE)
      ) %>%
    slice(-1)

  return(data)
}

#function to remove the last row containing extra data
remove_last_row <- function(data){
  data <- data %>%
    slice(-nrow(data))
  
  return(data)
}

#function to save data as RDS
save_data <- function(data){
  saveRDS(
    object=get(data),
    file=paste0("./data/",data,".rds")
  )
}

#function to read data from RDS
read_data <- function(){
  # readRDS(
  #   file=paste0("./data/",
  #               list.files(path="./data",pattern="_raw.rds"))
  # )
  
  # filepath <- "./data/"

  # matches_raw <- readRDS(paste0(filepath,"matches_raw.rds"))
  # squad_keepers_raw <- readRDS(paste0(filepath,"squad_keepers_raw.rds"))
  # squad_misc_raw <- readRDS(paste0(filepath,"squad_misc_raw.rds"))
  # squad_passing_raw <- readRDS(paste0(filepath,"squad_passing_raw.rds"))
  # squad_playingtime_raw <- readRDS(paste0(filepath,"squad_playingtime_raw.rds"))
  # squad_shooting_raw <- readRDS(paste0(filepath,"squad_shooting_raw.rds"))
  # squad_standard_raw <- readRDS(paste0(filepath,"squad_standard_raw.rds"))
  # table_raw <- readRDS(paste0(filepath,"table_raw.rds"))
}

# readRDS(paste0("./data/","matches_raw.rds"))
# readRDS(paste0("./data/",
#                list.files(path="./data",pattern="_raw.rds")))

# list.files(path="./data",pattern="_raw.rds")
# paste0("./data/",
#        list.files(path="./data",pattern="_raw.rds"))

# https://stats.idre.ucla.edu/r/codefragments/read_multiple/