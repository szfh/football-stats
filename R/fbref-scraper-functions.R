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
    browser()
    data_text <- data_text %>%
      enframe() %>%
      slice(-c(length(data_text)-skip_tail+1:length(data_text))) %>%
      deframe
    browser()
  }
  
  data <- data_text %>%
    matrix(ncol=ncol,byrow=T) %>%
    as_tibble()
  
  if(fix_columns==TRUE){
    data <- first_row_to_columns(data)
  }
  
  if(remove_last_row==TRUE){
    data <- remove_last_row(data)
  }
  
  return(data)
}

#function to make the first row of a tibble into the column names, then delete the first row
first_row_to_columns <- function(data){
  data <- data %>%
    set_colnames(data[1,]) %>%
    slice(-1)

  return(data)
}

#function to remove the last row containing extra data
remove_last_row <- function(data){
  data <- data %>%
    slice(-nrow(data))
  
  return(data)
}