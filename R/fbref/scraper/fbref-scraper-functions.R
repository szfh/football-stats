fbref_scrape <- function(url,nodes=NA,comment=FALSE,fix_columns=FALSE,extract=NA){
  
  Sys.sleep(1)
  
  if(comment==TRUE){ # table is inside html comment
    data_table <-
      read_html(url) %>%
      html_nodes(xpath="//comment()") %>%
      html_text() %>%
      paste(collapse="") %>%
      read_html() %>%
      html_nodes("table") %>%
      html_table
  } else { # table not inside html comment
    data_table <-
      read_html(url) %>%
      html_nodes("table") %>%
      html_table()
  }
  
  if(is.na(extract)==FALSE){ # sekect table
    data_table <- data_table %>%
      extract2(extract)
  }
  
  if(fix_columns==TRUE){
    names(data_table) <- data_table[1,] # move first row to names
    data_table <- as_tibble(data_table, .name_repair="unique") # create tibble
    data_table <- data_table %>% slice(-1) # remove first row
  } else {
    data_table <- as_tibble(data_table, .name_repair = "unique") # create tibble
  }
  
  if("Player" %in% names(data_table)){ # remove duplicated column names from player table
    data_table <- data_table %>%
      filter(Player != "Player")
  }
  
  data_table <- type_convert(data_table) # reset data types
  
  return(data_table)
}

#function to import from local CSV file
fbref_import <- function(file,skip=0){
  data_csv <- read_csv(file,skip=skip)
  
  return(data_csv)
}