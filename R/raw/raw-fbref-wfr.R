source(here("R","raw","raw-fbref-utils.R"),encoding="utf-8")

scrape_fbref_wfr <- function(save_path=here("data","fbref.rds"),current_season=2021){
  data_types <- list()
  data_types$season <- tibble(season=2021)
  data_types$country <- tibble(country="ENG")
  data_types$gender <- tibble(gender="M")
  data_types$advanced_stats <- tibble(stat=c("summary","passing","passing_types","defense" ,"possession","misc","keeper"))
  data_types$season_team_stats <- tibble(stat=c("league_table", "league_table_home_away", "standard", "keeper",
                                                "keeper_adv", "shooting", "passing", "passing_types", "goal_shot_creation",
                                                "defense", "possession", "playing_time", "misc"))
  data_types$team_or_player <- tibble(team_or_player=c("player","team"))
  
  fbref_saved <- readRDS(save_path)
  fbref <- list()

  fbref$match_urls$all <-
    tibble() %>%
    bind_rows(
      crossing(data_types$season,data_types$country,data_types$gender)
    ) %>%
    mutate(data_type="match_url") %>%
    mutate(data=pmap(list(country,gender,season),get_match_urls)) %>%
    mutate(data=map(data,as_tibble))

  fbref$match_results$all <-
    tibble() %>%
    bind_rows(
      crossing(data_types$season,data_types$country,data_types$gender)
    ) %>%
    mutate(data_type="match_result")
  
  fbref$match_results$keep <-
    fbref_saved %>%
    filter(data_type=="match_result") %>%
    filter(season!=current_season)
  
  fbref$match_results$new <-
    anti_join(fbref$match_results$all, fbref$match_results$keep) %>%
    mutate(data=pmap(list(country,gender,season),possibly(get_match_results,otherwise=NA)))

  fbref$season_stats$all <-
    tibble() %>%
    bind_rows(
      crossing(data_types$season,data_types$country,data_types$gender,data_types$season_team_stats)
    ) %>%
    mutate(data_type="season_stat")
  
  fbref$season_stats$keep <-
    fbref_saved %>%
    filter(data_type=="season_stat") %>%
    filter(season!=current_season)
  
  fbref$season_stats$new <-
    anti_join(fbref$season_stats$all, fbref$season_stats$keep) %>%
    mutate(data=pmap(list(country,gender,season,stat),possibly(get_season_team_stats,otherwise=NA)))

  fbref$match_summary$all <-
    fbref$match_urls$all %>%
    unnest(cols=data) %>%
    remove_empty("cols") %>%
    rename(url=value) %>%
    mutate(data_type="match_summary")
  
  fbref$match_summary$keep <-
    fbref_saved %>%
    filter(data_type=="match_summary") %>%
    filter(season!=current_season)
  
  fbref$match_summary$new <-
    anti_join(fbref$match_summary$all, fbref$match_summary$keep) %>%
    mutate(data=map(url,possibly(get_match_summary,otherwise=NA)))
  
  fbref$advanced_stats$all <-
    fbref$match_urls$all %>%
    unnest(cols=data) %>%
    remove_empty("cols") %>%
    rename(url=value) %>%
    crossing(data_types$advanced_stats) %>%
    crossing(data_types$team_or_player) %>%
    mutate(data_type="advanced_stats")
  
  fbref$advanced_stats$keep <-
    fbref_saved %>%
    filter(data_type=="advanced_stats") %>%
    filter(season!=current_season)
  
  fbref$advanced_stats$new <-
    anti_join(fbref$advanced_stats$all, fbref$advanced_stats$keep) %>%
    mutate(data=pmap(list(url,stat,team_or_player),possibly(get_advanced_match_stats,otherwise=NA)))
  
  fbref_all <-
    bind_rows(
      fbref$match_results$keep,fbref$match_results$new,
      fbref$season_stats$keep,fbref$season_stats$new,
      fbref$match_summary$keep,fbref$match_summary$new,
      fbref$advanced_stats$keep,fbref$advanced_stats$new
    ) %>%
    relocate(data_type) %>%
    relocate(data,.after=last_col())
  
  saveRDS(fbref_all,file=save_path)
  
  return(fbref_all)
}