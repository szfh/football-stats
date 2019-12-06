# function to scrape fbref
fbref_scrape <- function(url,nodes,nrow=NULL,ncol=NULL){
  data <-
    url %>%
    read_html() %>%
    html_nodes(nodes) %>%
    html_text %>%
    matrix(nrow=nrow,ncol=ncol,byrow=T) %>%
    as_tibble()
  
  return(data)
}

#function to make the first row of a tibble into the column names, then delete the first row
first_row_to_columns <- function(data){
  data <- data %>%
    set_colnames(data[1,]) %>%
    slice(-1)

  return(data)
}