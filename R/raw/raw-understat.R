source(here("R","raw","raw-fbref-utils.R"),encoding="utf-8")
source(here("R","themes.R"),encoding="utf-8")

scrape_understat <- function(save_path=here("data","understat.rds"),current_season="2021"){
  
  data_types <- get_data_types()
  
  understat_saved <- readRDS(save_path)
  understat <- list()
  
  eplseasons <- tibble(season=as.character(2014:2021))
  
  understat$results$all <-
    tibble() %>%
    bind_rows(eplseasons)
  
  understat$results$keep <-
    understat_saved %>%
    filter(data_type=="results")
  
  understat$results$new <-
    anti_join(understat$results$all,understat$results$keep) %>%
    mutate(data=pmap(list("EPL",season),understat_league_match_results)) %>%
    mutate(data_type="results") %>%
    glimpse
  
  understat$shots$all <-
    tibble() %>%
    bind_rows(eplseasons)
  
  understat$shots$keep <-
    understat_saved %>%
    filter(data_type=="shots")
  
  understat$shots$new <-
    anti_join(understat$shots$all,understat$shots$keep) %>%
    mutate(data=pmap(list("EPL",season),understat_league_season_shots)) %>%
    mutate(data_type="shots") %>%
    glimpse
  
  understat_all <-
    bind_rows(
      understat$results$keep,understat$results$new,
      understat$shots$keep,understat$shots$new
    ) %>%
    glimpse
  
  saveRDS(understat_all,file=save_path)
  
  return(understat)
}
