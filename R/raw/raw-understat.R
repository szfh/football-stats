source(here("R","raw","raw-utils.R"),encoding="utf-8")
source(here("R","themes.R"),encoding="utf-8")

scrape_understat <- function(
    save_path_results=here("data","understat_results.rds"),
    save_path_team_stats=here("data","understat_team_stats.rds"),
    save_path_shots=here("data","understat_shots.rds"),
    current_season="2023",
    refresh_season_stats=FALSE
){
  
  data_types <- get_data_types()
  
  understat_results_saved <- readRDS(save_path_results)
  understat_team_stats_saved <- readRDS(save_path_team_stats)
  understat_shots_saved <- readRDS(save_path_shots)
  understat <- list()
  
  seasons <- tibble(season=as.character(2014:2023))
  leagues <- tibble(league=c("EPL","La liga","Ligue 1","Bundesliga","Serie A"))
  
  understat$results$all <-
    tibble() %>%
    bind_rows(
      crossing(seasons,leagues)
    )
  
  understat$results$keep <-
    understat_results_saved %>%
    filter(data_type=="results") %>%
    filter(season!=current_season)
  
  understat$results$new <-
    anti_join(understat$results$all,understat$results$keep) %>%
    mutate(data=pmap(list(league,season),understat_league_match_results)) %>%
    mutate(data_type="results") %>%
    mutate(date_scraped=Sys.Date()) %>%
    print(n=Inf)
  
  understat$team_meta$all <-
    bind_rows(understat$results$keep,understat$results$new) %>%
    select(data) %>%
    unnest(cols=data) %>%
    select(league,home_team,away_team) %>%
    pivot_longer(cols=c("home_team","away_team"),values_to="team",names_to=NULL) %>%
    unique() %>%
    arrange(league,team)
  
  understat$team_meta$keep <-
    understat_team_stats_saved %>%
    filter(data_type=="team_meta") %>%
    {if(refresh_season_stats) filter(., (as.numeric(year)+1)!=as.numeric(current_season)) else .}
  
  understat$team_meta$new <-
    anti_join(understat$team_meta$all,understat$team_meta$keep) %>%
    mutate(data=pmap(list(team),understat_team_meta)) %>%
    mutate(data_type="team_meta") %>%
    mutate(date_scraped=Sys.Date()) %>%
    print(n=Inf)
  
  understat$team_stats$all <-
    bind_rows(understat$team_meta$keep,understat$team_meta$new) %>%
    select(data) %>%
    unnest(data) %>%
    rename(team=team_name)
  
  understat$team_stats$keep <-
    understat_team_stats_saved %>%
    filter(data_type=="team_stats") %>%
    {if(refresh_season_stats) filter(., (year+1)!=current_season) else .}
  
  understat$team_stats$new <-
    anti_join(understat$team_stats$all,understat$team_stats$keep) %>%
    mutate(data=pmap(list(url),understat_team_stats_breakdown)) %>%
    mutate(data_type="team_stats") %>%
    mutate(date_scraped=Sys.Date()) %>%
    print(n=Inf)
  
  understat$shots <-
    worldfootballR::load_understat_league_shots("EPL")
  
  understat_results <-
    bind_rows(
      understat$results$keep,understat$results$new
    ) %>%
    filter(!is.na(data)) %>%
    relocate(data_type) %>%
    relocate(date_scraped,.after=last_col()) %>%
    relocate(data,.after=last_col())
  
  understat_team_stats <-
    bind_rows(
      understat$team_meta$keep,understat$team_meta$new,
      understat$team_stats$keep,understat$team_stats$new
    ) %>%
    filter(!is.na(data)) %>%
    relocate(data_type) %>%
    relocate(date_scraped,.after=last_col()) %>%
    relocate(data,.after=last_col())
  
  understat_shots <-
    understat$shots
  
  saveRDS(understat_results,file=save_path_results)
  saveRDS(understat_team_stats,file=save_path_team_stats)
  saveRDS(understat_shots,file=save_path_shots)
  
  return()
}
