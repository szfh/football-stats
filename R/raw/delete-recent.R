source(here("R","raw","raw-utils.R"),encoding="utf-8")
source(here("R","themes.R"),encoding="utf-8")

delete_recent <- function(save_path=here("data","fbref.rds"),days=3){
  fbref_saved <- readRDS(save_path)
  
  fbref <-
    fbref_saved %>%
    mutate(event_date=pmap(list(data,data_type),possibly(get_event_date,otherwise=NA))) %>%
    mutate(remove=ifelse(event_date>=date_scraped-days,TRUE,FALSE)) %>%
    mutate(remove=replace_na(remove,FALSE)) %>%
    filter(!remove) %>%
    select(-event_date,-remove)
  
  removed <-
    anti_join(fbref_saved,fbref) %>%
    print(n=Inf)
  
  saveRDS(fbref,file=save_path)
  return(fbref)
}

get_event_date <- function(data,data_type=NA){
  if(data_type=="match_lineups"){
    event_date <-
      data %>%
      select(Matchday) %>%
      distinct() %>%
      pull()
  } else if(data_type=="match_summary"){
    event_date <- 
      data %>%
      select(Match_Date) %>%
      distinct() %>%
      lubridate::parse_date_time("ymd")
  } else if(data_type=="match_shots"){
    event_date <- 
      data %>%
      select(Date) %>%
      distinct() %>%
      pull()
  } else if(data_type=="advanced_stats"){
    event_date <-
      data %>%
      select(Match_Date) %>%
      distinct() %>%
      lubridate::parse_date_time("ymd")
  } else {
    event_date <- NA
  }
  event_date <- as_date(event_date)
  return(event_date)
}
