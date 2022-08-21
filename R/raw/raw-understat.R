source(here("R","raw","raw-utils.R"),encoding="utf-8")
source(here("R","themes.R"),encoding="utf-8")

scrape_understat <- function(save_path_results=here("data","understat_results.rds"),save_path_shots=here("data","understat_shots.rds"),current_season="2023"){
  
  data_types <- get_data_types()
  
  understat_results_saved <- readRDS(save_path_results)
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
    glimpse
  
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
  
  understat_shots <-
    understat$shots
  
  saveRDS(understat_results,file=save_path_results)
  saveRDS(understat_shots,file=save_path_shots)
  
  return()
}
