# function to scrape fbref
fbref_scrape <- function(url,nodes,ncol=1,skip_head=0,skip_tail=0,fix_columns=FALSE){
  # browser()
  # data <-
  #   url %>%
  #   read_html() %>%
  #   html_nodes(as.character(nodes)) %>%
  #   html_text %>%
  #   matrix(ncol=ncol,byrow=T) %>%
  #   as_tibble(.name_repair="minimal")
  
  # browser()
  
  data_text <-
    url %>%
    read_html() %>% 
    html_nodes(nodes) %>% 
    html_text
    # tibble::enframe()
  
  # browser()
  
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
  
  data <- data_text #matrix
    # matrix(ncol=ncol,byrow=T)
  
  if(fix_columns==TRUE){
    data <- first_row_to_columns(data)
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