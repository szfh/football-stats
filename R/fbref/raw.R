# function
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

# scraper
if (!exists("raw",inherits=FALSE)){
  raw <- list()
}

raw[["fbref"]][["table"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/Premier-League-Stats",
                               extract=1,fix_columns=FALSE)

raw[["fbref"]][["matches"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/schedule/Premier-League-Fixtures",
                                 extract=1,fix_columns=FALSE)

raw[["fbref"]][["squad"]][["standard"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/stats/Premier-League-Stats",
                                             extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["keepers"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/keepers/Premier-League-Stats",
                                            extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["keepersadv"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/keepersadv/Premier-League-Stats",
                                            extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["shooting"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/shooting/Premier-League-Stats",
                                             extract=1,fix_columns=FALSE)
raw[["fbref"]][["squad"]][["passing"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/passing/Premier-League-Stats",
                                            extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["playingtime"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/playingtime/Premier-League-Stats",
                                                extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["misc"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/misc/Premier-League-Stats",
                                         extract=1,fix_columns=TRUE)

raw[["fbref"]][["player"]][["standard"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/stats/Premier-League-Stats",
                                              comment=TRUE,extract=1,fix_columns=TRUE)
raw[["fbref"]][["player"]][["keepers"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/keepers/Premier-League-Stats",
                                             comment=TRUE,extract=1,fix_columns=TRUE)
raw[["fbref"]][["player"]][["keepersadv"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/keepersadv/Premier-League-Stats",
                                             comment=TRUE,extract=1,fix_columns=TRUE)
raw[["fbref"]][["player"]][["shooting"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/shooting/Premier-League-Stats",
                                              comment=TRUE,extract=1,fix_columns=FALSE)
raw[["fbref"]][["player"]][["passing"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/passing/Premier-League-Stats",
                                             comment=TRUE,extract=1,fix_columns=TRUE)
raw[["fbref"]][["player"]][["playingtime"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/playingtime/Premier-League-Stats",
                                                 comment=TRUE,extract=1,fix_columns=TRUE)
raw[["fbref"]][["player"]][["misc"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/misc/Premier-League-Stats",
                                          comment=TRUE,extract=1,fix_columns=TRUE)

saveRDS(raw,file=here("data","raw-fbref.rds"))