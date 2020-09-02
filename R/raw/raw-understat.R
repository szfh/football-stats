scrape_understat <- function(save_path=here("data","understat-raw.rds")){
  
  understat_saved <- readRDS(save_path)
  
  eplseasons <- tribble(~season,
                        "2019",
                        "2018",
  )
  
  data_types_league <- tribble(~datatype,
                               "league",
  )
  
  tables_league <- tribble(~stattype, ~statselector,
                           "schedule","datesData",
                           "players","playersData",
                           # "teams","teamsData",
  )
  
  understat_all1 <- data.frame() %>% # schedule and player data
    bind_rows(crossing(data_types_league,tables_league)) %>%
    crossing(eplseasons)
  
  understat_keep1 <- understat_saved %>%
    filter(datatype=="league") %>%
    filter(season!=2020)
  
  understat_new1 <-
    anti_join(understat_all1, understat_keep1)
  
  understat_new1 %<>%
    mutate(data=pmap(list("EPL",season,statselector),possibly(understat_scrape_league, otherwise=NA)))
  
  ###
  
  match_id <- understat_saved %>%
    filter(stattype=="schedule") %>%
    select(season,data) %>%
    unnest(cols=data) %>%
    select(season,id,isResult) %>%
    # select(-c(datatype,stattype,statselector)) %>%
    glimpse
  
  data_types_match <- tribble(~datatype,
                              "stats",
                              "shots"
  )
  
  understat_all2 <- data.frame() %>% # match stat + shot data
    bind_rows(crossing(match_id,data_types_match) %>%
                filter(isResult==TRUE))
  
  understat_keep2 <- understat_saved %>%
    filter(datatype %in% c("stats","shots")) %>%
    filter(season!=2020)
  
  understat_new2 <-
    anti_join(understat_all2, understat_keep2)
  
  understat_new2 %<>%
    mutate(data=pmap(list(datatype,id),possibly(understat_scrape_match, otherwise=NA)))
  
  ###
  
  understat <- bind_rows(understat_keep1, understat_new1, understat_keep2, understat_new2) %>%
    filter(!is.na(data)) %>%
    relocate(data,.after=last_col())
  
  saveRDS(understat,file=save_path)
  
  return(understat)
}

understat_scrape_league <- function(league="EPL", year="2019", str){
  url <- glue("https://understat.com/league/{league}/{year}")
  print(glue("url: {url}"))
  
  data <-
    url %>%
    read_html() %>%
    html_nodes("script") %>%
    as.character() %>%
    stringr::str_subset(str) %>%
    stringi::stri_unescape_unicode() %>%
    stringr::str_extract("\\[.+\\]") %>%
    jsonlite::fromJSON(simplifyVector=TRUE)
  
  return(data)
}

understat_scrape_match <- function(datatype,id){
  
  if(datatype=="stats"){
    data <- get_match_stats(id)
  }
  if(datatype=="shots"){
    data <- get_match_shots(id)
  }
  
  return(data)
}
