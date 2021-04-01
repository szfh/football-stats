source(here("R","raw","raw-fbref-utils.R"),encoding="utf-8")
source(here("R","themes.R"),encoding="utf-8")

scrape_fbref_wfr <- function(save_path=here("data","fbref.rds"),current_season=2021){
  data_types <- get_data_types()
  
  fbref_saved <- readRDS(save_path)
  fbref <- list()
  
  fbref$match_urls$all <-
    tibble() %>%
    bind_rows(
      crossing(data_types$season,data_types$country)
    ) %>%
    mutate(data_type="match_url") %>%
    mutate(data=pmap(list(country,gender="M",season),get_match_urls)) %>%
    mutate(data=map(data,as_tibble)) %>%
    mutate(data=map(data,unique))
  
  fbref$match_results$all <-
    tibble() %>%
    bind_rows(
      crossing(data_types$season,data_types$country)
    ) %>%
    mutate(data_type="match_result")
  
  fbref$match_results$keep <-
    fbref_saved %>%
    filter(data_type=="match_result") %>%
    filter(season!=current_season) %>%
    filter(!is.na(data))
  
  fbref$match_results$new <-
    anti_join(fbref$match_results$all, fbref$match_results$keep) %>%
    mutate(data=pmap(list(country,gender="M",season,tier="1st"),possibly(get_match_results,otherwise=NA))) %>%
    print(n=Inf)
  
  fbref$season_stats$all <-
    tibble() %>%
    bind_rows(
      crossing(data_types$season,data_types$country,data_types$season_team_stats)
    ) %>%
    mutate(data_type="season_stat")
  
  fbref$season_stats$keep <-
    fbref_saved %>%
    filter(data_type=="season_stat") %>%
    filter(season!=current_season) %>%
    filter(!is.na(data))
  
  fbref$season_stats$new <-
    anti_join(fbref$season_stats$all, fbref$season_stats$keep) %>%
    mutate(data=pmap(list(country,gender="M",season,tier="1st",stat),possibly(get_season_team_stats,otherwise=NA))) %>%
    print(n=Inf)
  
  fbref$match_summary$all <-
    fbref$match_urls$all %>%
    unnest(cols=data) %>%
    rename(url=value) %>%
    mutate(data_type="match_summary")
  
  fbref$match_summary$keep <-
    fbref_saved %>%
    filter(data_type=="match_summary") %>%
    filter(!is.na(data))
  
  fbref$match_summary$new <-
    anti_join(fbref$match_summary$all, fbref$match_summary$keep) %>%
    mutate(data=map(url,possibly(get_match_summary,otherwise=NA))) %>%
    print(n=Inf)
  
  fbref$advanced_stats$all <-
    fbref$match_urls$all %>%
    unnest(cols=data) %>%
    rename(url=value) %>%
    crossing(data_types$advanced_stats) %>%
    crossing(data_types$team_or_player) %>%
    mutate(data_type="advanced_stats")
  
  fbref$advanced_stats$keep <-
    fbref_saved %>%
    filter(data_type=="advanced_stats") %>%
    filter(!is.na(data))
  
  fbref$advanced_stats$new <-
    anti_join(fbref$advanced_stats$all, fbref$advanced_stats$keep) %>%
    mutate(data=pmap(list(url,stat,team_or_player),possibly(get_advanced_match_stats,otherwise=NA))) %>%
    print(n=Inf)
  
  # browser()
  
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