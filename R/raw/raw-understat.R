source(here::here("R","library.R"))
source(here("R","raw","raw-utils.R"))

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
  
  understat_all1 <- data.frame() %>%
    bind_rows(crossing(data_types_league,tables_league)) %>%
    crossing(eplseasons)
  
  understat_keep1 <- understat_saved %>%
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
  
  understat_all2 <- data.frame() %>%
    bind_rows(crossing(match_id,data_types_match) %>%
                filter(isResult==TRUE)) %>%
    glimpse
  
  understat_keep2 <- understat_all2 %>%
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
