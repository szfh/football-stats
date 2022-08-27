source(here("R","raw","raw-utils.R"),encoding="utf-8")
source(here("R","themes.R"),encoding="utf-8")

scrape_fbref <- function(save_path=here("data","fbref.rds"),save_path_urls=here("data","fbref_urls.rds"),current_season=2023){
  data_types <- get_data_types()
  
  fbref_saved <- readRDS(save_path)
  fbref_urls_saved <- readRDS(save_path_urls)
  fbref <- list()
  fbref_urls <- list()
  
  fbref_urls$league$all <-
    tibble() %>%
    bind_rows(
      crossing(data_types$season,data_types$country)
    ) %>%
    mutate(data_type="league")
  
  fbref_urls$league$keep <-
    fbref_urls_saved %>%
    filter(data_type=="league")
  
  fbref_urls$league$new <-
    anti_join(fbref_urls$league$all, fbref_urls$league$keep) %>%
    mutate(data=pmap(list(country,gender="M",season),fb_league_urls)) %>%
    mutate(date_scraped=today()) %>%
    print(n=Inf)
  
  fbref_urls$team$all <-
    bind_rows(fbref_urls$league$keep,fbref_urls$league$new) %>%
    mutate(data_type="team") %>%
    rename(data_league=data)
  
  fbref_urls$team$keep <-
    fbref_urls_saved %>%
    filter(data_type=="team")
  
  fbref_urls$team$new <-
    anti_join(fbref_urls$team$all, fbref_urls$team$keep) %>%
    mutate(data=map(data_league,fb_teams_urls)) %>%
    select(-data_league) %>%
    mutate(date_scraped=today()) %>%
    print(n=Inf)
  
  fbref_urls$match$all <-
    bind_rows(fbref_urls$league$keep,fbref_urls$league$new) %>%
    mutate(data_type="match") %>%
    rename(data_league=data)
  
  fbref_urls$match$keep <-
    fbref_urls_saved %>%
    filter(data_type=="match") %>%
    filter(season!=current_season)
  
  fbref_urls$match$new <-
    anti_join(fbref_urls$match$all, fbref_urls$match$keep) %>%
    mutate(data=pmap(list(country,gender="M",season),fb_match_urls)) %>%
    select(-data_league) %>%
    mutate(date_scraped=today()) %>%
    print(n=Inf)
  
  fbref$match_results_league$all <-
    crossing(
      data_types$season,
      country="ENG"
    ) %>%
    mutate(data_type="match_result_league")
  
  fbref$match_results_league$keep <-
    fbref_saved %>%
    filter(data_type=="match_result_league") %>%
    filter(season!=current_season) %>%
    filter(!is.na(data))
  
  fbref$match_results_league$new <-
    anti_join(fbref$match_results_league$all, fbref$match_results_league$keep) %>%
    mutate(data=pmap(list(country,gender="M",season,tier="1st"),possibly(load_match_results,otherwise=NA))) %>%
    print(n=Inf)
  
  fbref$match_results_cup$all <-
    crossing(
      competition=
        c("FA Cup","English Football League Cup",
          "UEFA Champions League","UEFA Europa League","UEFA Europa Conference League")
    ) %>%
    mutate(data_type="match_result_cup")
  
  fbref$match_results_cup$keep <-
    fbref_saved %>%
    filter(data_type=="match_result_cup") %>%
    filter(!is.na(season)) %>%
    filter(!is.na(data))
  
  fbref$match_results_cup$new <-
    anti_join(fbref$match_results_cup$all, fbref$match_results_cup$keep) %>%
    mutate(data=pmap(list(competition),possibly(load_match_comp_results,otherwise=NA))) %>%
    print(n=Inf)
  
  fbref$season_stats$all <-
    crossing(
      data_types$season,
      team_or_player=c("team","player"),
      stat=c("standard","shooting","passing","passing_types","gca","defense","possession","playing_time","misc","keepers","keepers_adv")
    ) %>%
    mutate(data_type="season_stat")
  
  fbref$season_stats$keep <-
    fbref_saved %>%
    filter(data_type=="season_stat") %>%
    filter(season!=current_season) %>%
    filter(!is.na(data))
  
  fbref$season_stats$new <-
    anti_join(fbref$season_stats$all, fbref$season_stats$keep) %>%
    mutate(data=pmap(list(season,stat,team_or_player),load_fb_big5_advanced_season_stats)) %>%
    print(n=Inf)
  
  fbref$match_summary$all <-
    bind_rows(fbref_urls$match$keep,fbref_urls$match$new) %>%
    select(-date_scraped) %>%
    unnest(cols=data) %>%
    rename(url=data) %>%
    mutate(data_type="match_summary")
  
  fbref$match_summary$keep <-
    fbref_saved %>%
    filter(data_type=="match_summary") %>%
    filter(!is.na(data))
  
  fbref$match_summary$new <-
    anti_join(fbref$match_summary$all, fbref$match_summary$keep) %>%
    mutate(data=map(url,possibly(fb_match_summary,otherwise=NA))) %>%
    mutate(date_scraped=today()) %>%
    print(n=Inf)
  
  fbref$match_lineups$all <-
    bind_rows(fbref_urls$match$keep,fbref_urls$match$new) %>%
    select(-date_scraped) %>%
    unnest(cols=data) %>%
    rename(url=data) %>%
    mutate(data_type="match_lineups")
  
  fbref$match_lineups$keep <-
    fbref_saved %>%
    filter(data_type=="match_lineups") %>%
    filter(!is.na(data))
  
  fbref$match_lineups$new <-
    anti_join(fbref$match_lineups$all, fbref$match_lineups$keep) %>%
    mutate(data=map(url,possibly(fb_match_lineups,otherwise=NA))) %>%
    mutate(date_scraped=today()) %>%
    print(n=Inf)
  
  fbref$match_shots$all <-
    bind_rows(fbref_urls$match$keep,fbref_urls$match$new) %>%
    select(-date_scraped) %>%
    unnest(cols=data) %>%
    rename(url=data) %>%
    mutate(data_type="match_shots")
  
  fbref$match_shots$keep <-
    fbref_saved %>%
    filter(data_type=="match_shots") %>%
    filter(!is.na(data)) %>%
    filter(map(data,length)>0)
  
  fbref$match_shots$new <-
    anti_join(fbref$match_shots$all, fbref$match_shots$keep) %>%
    mutate(data=map(url,possibly(fb_match_shooting,otherwise=NA))) %>%
    mutate(date_scraped=today()) %>%
    print(n=Inf)
  
  fbref$advanced_stats$all <-
    bind_rows(fbref_urls$match$keep,fbref_urls$match$new) %>%
    select(-date_scraped) %>%
    unnest(cols=data) %>%
    rename(url=data) %>%
    crossing(data_types$advanced_stats) %>%
    crossing(data_types$team_or_player) %>%
    mutate(data_type="advanced_stats")
  
  fbref$advanced_stats$keep <-
    fbref_saved %>%
    filter(data_type=="advanced_stats") %>%
    filter(!is.na(data))
  
  fbref$advanced_stats$new <-
    anti_join(fbref$advanced_stats$all, fbref$advanced_stats$keep) %>%
    filter(season %in% c(2021,2022,2023)) %>%
    mutate(data=pmap(list(url,stat,team_or_player),possibly(fb_advanced_match_stats,otherwise=NA))) %>%
    mutate(date_scraped=today()) %>%
    print(n=Inf)
  
  fbref_all <-
    bind_rows(
      fbref$match_results_league$keep,fbref$match_results_league$new,
      fbref$match_results_cup$keep,fbref$match_results_cup$new,
      fbref$season_stats$keep,fbref$season_stats$new,
      fbref$match_lineups$keep,fbref$match_lineups$new,
      fbref$match_summary$keep,fbref$match_summary$new,
      fbref$match_shots$keep,fbref$match_shots$new,
      fbref$advanced_stats$keep,fbref$advanced_stats$new,
    ) %>%
    filter(!is.na(data)) %>%
    relocate(data_type) %>%
    relocate(date_scraped,.after=last_col()) %>%
    relocate(data,.after=last_col())
  
  fbref_urls_all <-
    bind_rows(
      fbref_urls$league$keep,fbref_urls$league$new,
      fbref_urls$team$keep,fbref_urls$team$new,
      fbref_urls$match$keep,fbref_urls$match$new,
    ) %>%
    filter(!is.na(data)) %>%
    relocate(data_type) %>%
    relocate(date_scraped,.after=last_col()) %>%
    relocate(data,.after=last_col())
  
  saveRDS(fbref_all,file=save_path)
  saveRDS(fbref_urls_all,file=save_path_urls)
  
  return(fbref_all)
}
