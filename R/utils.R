# scrape fbref
fbref_scrape <- function(url,comment=FALSE,fix_columns=FALSE,extract=NA){
  
  Sys.sleep(0.1)
  
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
  
  if(is.na(extract)==FALSE){ # select table
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

# windowed average xG
get_mva <- function(xG,n=6){
  
  xGlag <- list()
  xGlag[[1]] <- xG
  
  for(i in 2:n){
    xGlag[[i]] <- lag(xG,(i-1))
  }
  
  mva <- xGlag %>%
    as.data.frame %>%
    rowMeans(na.rm=TRUE)
  
  return(mva)
}

# latest_data <- matches %>%
#   filter(!is.na(GoalsHome)&!is.na(GoalsAway)) %>%
#   summarise(last(Date)) %>%
#   extract2(1)
