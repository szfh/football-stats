fbref_scrape <- function(page_url,content_selector_id){
  url <- glue("http://acciotables.herokuapp.com/?page_url={page_url}&content_selector_id={content_selector_id}")
  print(glue("url: {url}"))
  
  data <-
    url %>%
    read_html() %>%
    html_table(header=FALSE) %>%
    extract2(1)
  
  return(data)
}

fbref_clean_names <- function(data){
  
  # names(data) <- make_clean_names(str_squish(glue("{data[1,]} {data[2,]}")))
  names(data) <-
    glue("{data[1,]} {data[2,]}") %>%
    str_squish %>%
    make_clean_names
  data %<>% slice(-1,-2)
  
  if("player" %in% names(data)){ # remove duplicated column names from player table
    data %<>% filter(player != "Player")
  }
  
  data %<>% type_convert # refactor data types
  
  return(data)
}

fbref_tidy <- function(data,page,type,cols){ #all data editing, selecting, renaming in here?
  
  if(type %in% c("squad","player")){
    data %<>% select(-any_of(c("rk","matches")))
  }
  if(page != "stats"){
    data %<>% select(-any_of("number_pl"))
  }
  if(page %in% c("keepers","keepersadv")){
    data %<>% select(-any_of(c("playing_time_starts","playing_time_mp","playing_time_min")))
  }
  return(data)
}

# scrape fbref old
fbref_scrape_old <- function(url,comment=FALSE,fix_columns=FALSE,extract=NA){
  
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
