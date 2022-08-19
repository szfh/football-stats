source(here("R","raw","raw-utils.R"),encoding="utf-8")
source(here("R","themes.R"),encoding="utf-8")

scrape_understat <- function(save_path=here("data","understat.rds"),current_season="2023",include_shots=FALSE){
  
  data_types <- get_data_types()
  
  understat_saved <- readRDS(save_path)
  understat <- list()
  
  seasons <- tibble(season=as.character(2014:2023))
  leagues <- tibble(league=c("EPL","La liga","Ligue 1","Bundesliga","Serie A"))
  
  understat$results$all <-
    tibble() %>%
    bind_rows(
      crossing(seasons,leagues)
    )
  
  understat$results$keep <-
    understat_saved %>%
    filter(data_type=="results") %>%
    filter(season!=current_season)
  
  understat$results$new <-
    anti_join(understat$results$all,understat$results$keep) %>%
    mutate(data=pmap(list(league,season),understat_league_match_results)) %>%
    mutate(data_type="results") %>%
    mutate(date_scraped=Sys.Date()) %>%
    glimpse
  
  # understat$team_stats$all <-
  #   tibble() %>%
  #   bind_rows(
  #     crossing(seasons,league="EPL")
  #   )
  
  # understat$team_stats$keep <-
  #   understat_saved %>%
  #   filter(data_type=="team_stats") %>%
  #   filter(season!=current_season)
  
  # understat$team_stats$new <-
  #   anti_join(understat$team_stats$all,understat$team_stats$keep) %>%
  #   mutate(data=pmap(list(league,season),understatr::get_league_teams_stats)) %>%
  #   mutate(data_type="team_stats") %>%
  #   mutate(date_scraped=Sys.Date()) %>%
  #   glimpse
  
  understat$shots$all <-
    tibble() %>%
    bind_rows(
      crossing(seasons,leagues)
    )
  
  understat$shots$keep <-
    understat_saved %>%
    filter(data_type=="shots") %>%
    {if(include_shots) filter(., season!=current_season) else .}
  
  understat$shots$new <-
    anti_join(understat$shots$all,understat$shots$keep) %>%
    filter(league=="EPL") %>%
    mutate(data=pmap(list(league,season),understat_league_season_shots)) %>%
    mutate(data_type="shots") %>%
    mutate(date_scraped=Sys.Date()) %>%
    glimpse
  
  understat_all <-
    bind_rows(
      understat$results$keep,understat$results$new,
      # understat$match_stats$keep,understat$match_stats$new,
      understat$shots$keep,understat$shots$new
    ) %>%
    filter(!is.na(data)) %>%
    relocate(data_type) %>%
    relocate(date_scraped,.after=last_col()) %>%
    relocate(data,.after=last_col())
  
  saveRDS(understat_all,file=save_path)
  
  return(understat_all)
}
