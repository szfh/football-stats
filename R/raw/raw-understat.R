source(here("R","raw","raw-fbref-utils.R"),encoding="utf-8")
source(here("R","themes.R"),encoding="utf-8")

scrape_understat <- function(save_path=here("data","understat.rds"),current_season="2022"){
  
  data_types <- get_data_types()
  
  understat_saved <- readRDS(save_path)
  understat <- list()
  
  seasons <- tibble(season=as.character(2014:2022))
  leagues <- tibble(league=c("EPL","La liga","Ligue 1","Bundesliga","Serie A"))
  
  understat$results$all <-
    tibble() %>%
    bind_rows(
      crossing(seasons,leagues)
    )
  
  understat$results$keep <-
    understat_saved %>%
    filter(data_type=="results")
  
  understat$results$new <-
    anti_join(understat$results$all,understat$results$keep) %>%
    mutate(data=pmap(list(league,season),understat_league_match_results)) %>%
    mutate(data_type="results") %>%
    mutate(date_scraped=Sys.Date()) %>%
    glimpse
  
  understat$shots$all <-
    tibble() %>%
    bind_rows(
      crossing(seasons,leagues)
    )
  
  understat$shots$keep <-
    understat_saved %>%
    filter(data_type=="shots")
  
  understat$shots$new <-
    anti_join(understat$shots$all,understat$shots$keep) %>%
    mutate(data=pmap(list(league,season),understat_league_season_shots)) %>%
    mutate(data_type="shots") %>%
    mutate(date_scraped=Sys.Date()) %>%
    glimpse
  
  understat_all <-
    bind_rows(
      understat$results$keep,understat$results$new,
      understat$shots$keep,understat$shots$new
    ) %>%
    filter(!is.na(data)) %>%
    relocate(data_type) %>%
    relocate(date_scraped,.after=last_col()) %>%
    relocate(data,.after=last_col())
  
  saveRDS(understat_all,file=save_path)
  
  return(understat_all)
}
