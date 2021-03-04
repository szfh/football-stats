source(here("R","raw","raw-fbref-utils.R"),encoding="utf-8")

scrape_fbref_wfr <- function(save_path=here("data","fbref.rds"),current_season=2021){
  data_types <- list()
  data_types$season <- tibble(season=2021)
  data_types$country <- tibble(country="ENG")
  data_types$gender <- tibble(gender="M")
  data_types$advanced_stats_short <- tibble(stat=c("summary"))
  data_types$advanced_stats <- tibble(stat=c("summary","passing","passing_types","defense" ,"possession","misc","keeper"))
  data_types$season_team_stats <- tibble(stat=c("league_table", "league_table_home_away", "standard", "keeper",
                                                "keeper_adv", "shooting", "passing", "passing_types", "goal_shot_creation",
                                                "defense", "possession", "playing_time", "misc"))
  data_types$season_team_stats_short <- tibble(stat=c("standard"))
  data_types$team_or_player <- tibble(team_or_player=c("player","team"))
  
  fbref_saved <- readRDS(save_path)
  fbref <- list()
  
  browser()
  
  fbref$match_urls$all <-
    tibble() %>%
    bind_rows(
      crossing(data_types$season,data_types$country,data_types$gender)
    ) %>%
    mutate(data_type="match_url") %>%
    glimpse
  
  fbref$match_urls$keep <-
    fbref_saved %>%
    filter(data_type=="match_url") %>%
    filter(season!=current_season) %>%
    glimpse
  
  fbref$match_urls$new <-
    anti_join(fbref$match_urls$all, fbref$match_urls$keep) %>%
    mutate(data=pmap(list(country,gender,season),get_match_urls)) %>%
    mutate(data=map(data,as_tibble)) %>%
    glimpse
  
  fbref$match_results$all <-
    tibble() %>%
    bind_rows(
      crossing(data_types$season,data_types$country,data_types$gender)
    ) %>%
    mutate(data_type="match_result") %>%
    glimpse
  
  fbref$match_results$keep <-
    fbref_saved %>%
    filter(data_type=="match_result") %>%
    filter(season!=current_season) %>%
    glimpse
  
  fbref$match_results$new <-
    anti_join(fbref$match_results$all, fbref$match_results$keep) %>%
    mutate(data=pmap(list(country,gender,season),get_match_results)) %>%
    glimpse
  
  # browser()
  
  fbref$season_stats$all <-
    tibble() %>%
    bind_rows(
      crossing(data_types$season,data_types$country,data_types$gender,data_types$season_team_stats_short)
    ) %>%
    mutate(data_type="season_stat") %>%
    glimpse
  
  fbref$season_stats$keep <-
    fbref_saved %>%
    filter(data_type=="season_stat") %>%
    filter(season!=current_season) %>%
    glimpse
  
  fbref$season_stats$new <-
    anti_join(fbref$season_stats$all, fbref$season_stats$keep) %>%
    mutate(data=pmap(list(country,gender,season,stat="standard"),get_season_team_stats)) %>%
    glimpse
  
  browser()
  
  # fbref_match_list <- list()
  
  # fbref$match_summary <-
  #   fbref$match_urls %>%
  #   unnest(cols=data) %>%
  #   rename(url=value) %>%
  #   slice(1:4) %>% # delete
  #   mutate(data=map(url,get_match_summary)) %>%
  #   glimpse
  
  # fbref$advanced_match_stats <-
  #   fbref$match_urls %>%
  #   unnest(cols=data) %>%
  #   rename(url=value) %>%
  #   slice(1:4) %>% # delete
  #   crossing(data_types$advanced_stats_short) %>%
  #   # crossing(data_types$advanced_stats) %>%
  #   # crossing(data_types$team_or_player) %>%
  #   crossing(tibble(team_or_player="player")) %>%
  #   mutate(data=pmap(list(url,stat,team_or_player),get_advanced_match_stats)) %>%
  #   glimpse
  
  # fbref1 <-
  #   bind_rows(fbref$match_urls,fbref$match_results) %>%
  #   relocate(data,.after=last_col())
  
  # fbref2 <-
  #   bind_rows(fbref$match_summary,fbref$advanced_match_stats) %>%
  # relocate(data,.after=last_col())
  
  # fbref_all <-
  #   bind_rows(fbref1,fbref2) %>%
  #   relocate(data_type,.before=first_col()) %>%
  #   relocate(data,.after=last_col())
  
  browser()
  
  glimpse(fbref)
  
  fbref_all <-
    bind_rows(
      fbref$match_urls$keep,fbref$match_urls$new,
      fbref$match_results$keep,fbref$match_results$new,
      fbref$season_stats$keep,fbref$season_stats$new
    ) %>%
    relocate(data_type) %>%
    relocate(data,.after=last_col()) %>%
    glimpse
  
  saveRDS(fbref_all,file=save_path)
  
  return(fbref_all)
}