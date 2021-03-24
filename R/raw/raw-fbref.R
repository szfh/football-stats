source(here("R","raw","raw-fbref-utils.R"),encoding="utf-8")

scrape_fbref <- function(save_path=here("data","fbref.rds"),current_season="2020-21"){
  fbref_saved <- readRDS(save_path)
  
  data_types <- list()
  
  data_types$seasons <- tribble(~season, ~season_key, #advanced/non-advanced?
                                "2020-21",10728,
                                "2019-20",3232,
                                "2018-19",1889,
                                "2017-18",1631,
                                # "2016-17",1526,
  )
  
  data_types$squad_player <- tribble(~page,
                                     "player",
                                     "squad",
  )
  
  data_types$squad_player_tables <- tribble(~stat, ~stat_key, # make factor/ordered?
                                            "stats","standard",
                                            "keepers","keeper",
                                            "keepersadv","keeper_adv",
                                            "shooting","shooting",
                                            "passing","passing",
                                            "passing_types","passing_types",
                                            "gca","gca",
                                            "defense","defense",
                                            "possession","possession",
                                            "playingtime","playing_time",
                                            "misc","misc",
  )
  
  data_types$league <- tribble(~page,
                               "schedule",
                               "league",
                               "leagueha",
  )
  
  fbref_data <- list()
  fbref <- list()
  
  browser()
  
  fbref_data$squad_player$all <-
    tibble() %>% # all squad + player data parameters
    bind_rows(crossing(data_types$squad_player,data_types$squad_player_tables)) %>%
    bind_rows(data_types$league) %>%
    crossing(data_types$seasons)
  
  fbref_data$squad_player$keep <-
    fbref_saved %>%
    filter(page %in% c("player","squad","league","leagueha","schedule")) %>%
    filter(season!=current_season)
  
  fbref_data$squad_player$new <-
    anti_join(fbref_data$squad_player$all, fbref_data$squad_player$keep) %>%
    mutate(page_url=fbref_get_url(page,season_key,stat,stat_key)) %>% # don't need stat?
    mutate(content_selector_id=fbref_get_selector(page,season_key,stat,stat_key)) %>% #don't need stat?
    mutate(data=pmap(list(page_url, content_selector_id, page, stat), possibly(fbref_scrape, otherwise=NA)))
  
  # browser()
  
  fbref_data$matches$all <-
    fbref_saved %>%
    filter(page=="schedule") %>%
    select(page,stat,stat_key,season,season_key,page_url,content_selector_id,data) %>%
    unnest(cols=data) %>%
    select(page:season_key,home,away,match_key=code) %>% # fix match_key=code?
    mutate(page="match",stat="events") %>% # crossing?
    filter(!is.na(match_key))
  
  fbref_data$events$keep <-
    fbref_saved %>%
    filter(stat=="events") %>%
    filter(!is.na(data)) %>%
    filter(!is.null(data))
  
  fbref_data$events$new <-
    anti_join(fbref_data$matches$all, fbref_data$events$keep, by="match_key") %>%
    mutate(stat="events") %>% #delete?
    mutate(page_url=fbref_get_url(page,match_key=match_key)) %>%
    mutate(data=pmap(list(page_url,page,stat), possibly(fbref_scrape_events, otherwise=NA)))
  
  fbref_data$shots$keep <-
    fbref_saved %>%
    filter(stat=="shots") %>%
    filter(!is.na(data)) %>%
    filter(!is.null(data))
  
  fbref_data$shots$new <-
    anti_join(fbref_data$matches$all, fbref_data$shots$keep, by="match_key") %>%
    mutate(stat="shots") %>%
    mutate(page_url=fbref_get_url(page,match_key=match_key)) %>%
    mutate(content_selector_id=fbref_get_selector(page,stat=stat,match_key=match_key)) %>%
    mutate(data=pmap(list(page_url,content_selector_id,page,stat), possibly(fbref_scrape, otherwise=NA)))
  
  browser()
  
  fbref_all <-
    bind_rows(
      fbref$squad_player$keep,fbref$squad_player$new,
      fbref$events$keep,fbref$events$new,
      fbref$shots$keep,fbref$shots$new
    ) %>%
    relocate(data,.after=last_col())
  
  browser()
  
  saveRDS(fbref_all,file=save_path)
  
  return(fbref_all)
}