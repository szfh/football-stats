source(here("R","join","tidy.R"),encoding="utf-8")

join <- function(
    save_path_fbref=here("data","fbref.rds"),
    save_path_fbref_urls=here("data","fbref_urls.rds"),
    save_path_understat_results=here("data","understat_results.rds"),
    save_path_understat_team_stats=here("data","understat_team_stats.rds"),
    save_path_understat_shots=here("data","understat_shots.rds"),
    save_path_fivethirtyeight=here("data","fivethirtyeight.rds"),
    save_path_canpl=here("data","canpl.rds")
){
  fbref_join <- possibly(join_fbref, otherwise=NA)(readRDS(save_path_fbref))
  fbref_urls_join <- possibly(join_fbref_urls, otherwise=NA)(readRDS(save_path_fbref_urls))
  understat_results_join <- possibly(join_understat_results, otherwise=NA)(readRDS(save_path_understat_results))
  understat_team_stats_join <- possibly(join_understat_team_stats, otherwise=NA)(readRDS(save_path_understat_team_stats))
  understat_shots_join <- possibly(join_understat_shots, otherwise=NA)(readRDS(save_path_understat_shots))
  fivethirtyeight_join <- possibly(join_fivethirtyeight, otherwise=NA)(readRDS(save_path_fivethirtyeight))
  canpl_join <- possibly(join_canpl, otherwise=NA)(readRDS(save_path_canpl))
  data <- list(
    fbref=fbref_join,
    fbref_urls=fbref_urls_join,
    understat_results=understat_results_join,
    understat_team_stats=understat_team_stats_join,
    understat_shots=understat_shots_join,
    fivethirtyeight=fivethirtyeight_join,
    canpl=canpl_join
  )
  
  return(data)
}

join_fbref <- function(fbref){
  fbref_tidy <-
    fbref %>%
    mutate(data=map(data,as_tibble)) %>%
    mutate(data=pmap(list(data,data_type,stat,team_or_player),possibly(tidy_fbref, otherwise=NA)))
  
  errors <-
    fbref_tidy %>%
    filter(is.na(data)) %>%
    print(n=Inf)
  
  data <- list()
  data$match_results_league <-
    fbref_tidy %>%
    filter(data_type=="match_result_league") %>%
    # select(data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$match_results_cup <-
    fbref_tidy %>%
    filter(data_type=="match_result_cup") %>%
    # select(season,data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$match_summary <-
    fbref_tidy %>%
    filter(data_type=="match_summary") %>%
    # select(data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$match_lineups <-
    fbref_tidy %>%
    filter(data_type=="match_lineups") %>%
    # select(data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$match_shots <-
    fbref_tidy %>%
    filter(data_type=="match_shots") %>%
    # select(data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$table <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="league_table") %>%
    # select(data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$table_home_away <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="league_table_home_away") %>%
    # select(data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$season_stat_defense <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="defense") %>%
    # select(data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$season_stat_gca <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="goal_shot_creation") %>%
    # select(data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$season_stat_keeper <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="keeper") %>%
    # select(data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$season_stat_keeper_adv <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="keeper_adv") %>%
    # select(data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$season_stat_misc <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="misc") %>%
    # select(data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$season_stat_passing <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="passing") %>%
    # select(data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$season_stat_passing_types <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="passing_types") %>%
    # select(data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$season_stat_possession <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="possession") %>%
    # select(data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$season_stat_playing_time <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="playing_time") %>%
    # select(data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$season_stat_shooting <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="shooting") %>%
    # select(data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$season_stat_standard <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="standard") %>%
    # select(data) %>%
    unnest(data) %>%
    janitor::remove_empty("cols")
  
  data$advanced_stats_player_possession <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="possession" & team_or_player=="player") %>%
    # select(data) %>%
    unnest(data)
  
  data$advanced_stats_player_passing <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="passing" & team_or_player=="player") %>%
    # select(data) %>%
    unnest(data)
  
  data$advanced_stats_player_summary <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="summary" & team_or_player=="player") %>%
    # select(data) %>%
    unnest(data)
  
  data$advanced_stats_player_defense <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="defense" & team_or_player=="player") %>%
    # select(data) %>%
    unnest(data)
  
  data$advanced_stats_player_misc <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="misc" & team_or_player=="player") %>%
    # select(data) %>%
    unnest(data)
  
  data$advanced_stats_player_passing_types <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="passing_types" & team_or_player=="player") %>%
    # select(data) %>%
    unnest(data)
  
  data$advanced_stats_player_keeper <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="keeper" & team_or_player=="player") %>%
    # select(data) %>%
    unnest(data)
  
  data$advanced_stats_team_possession <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="possession" & team_or_player=="team") %>%
    # select(data) %>%
    unnest(data)
  
  data$advanced_stats_team_passing <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="passing" & team_or_player=="team") %>%
    # select(data) %>%
    unnest(data)
  
  data$advanced_stats_team_summary <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="summary" & team_or_player=="team") %>%
    # select(data) %>%
    unnest(data)
  
  data$advanced_stats_team_defense <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="defense" & team_or_player=="team") %>%
    # select(data) %>%
    unnest(data)
  
  data$advanced_stats_team_misc <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="misc" & team_or_player=="team") %>%
    # select(data) %>%
    unnest(data)
  
  data$advanced_stats_team_passing_types <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="passing_types" & team_or_player=="team") %>%
    # select(data) %>%
    unnest(data)
  
  data$advanced_stats_team_keeper <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="keeper" & team_or_player=="team") %>%
    # select(data) %>%
    unnest(data)
  
  return(data)
}

join_fbref_urls <- function(fbref_urls){
  data <-
    fbref_urls
  
  return(data)
}

join_understat_results <- function(understat_results){
  
  data <-
    understat_results %>%
    filter(data_type=="results") %>%
    select(data) %>%
    unnest(data) %>%
    distinct() %>%
    mutate(date=parse_date_time(datetime,"ymd HMS"),.keep="unused",.after="season")
  
  return(data)
}

join_understat_shots <- function(understat_shots){
  
  data <-
    understat_shots %>%
    distinct() %>%
    unite("h_a",h_a,home_away,na.rm=TRUE,remove=TRUE) %>%
    unite("x",x,X,na.rm=TRUE,remove=TRUE) %>%
    unite("y",y,Y,na.rm=TRUE,remove=TRUE) %>%
    unite("xG",xG,x_g,na.rm=TRUE,remove=TRUE) %>%
    unite("shotType",shot_type,shotType,na.rm=TRUE,remove=TRUE) %>%
    unite("lastAction",last_action,lastAction,na.rm=TRUE,remove=TRUE) %>%
    mutate(date=parse_date_time(date,"ymd HMS")) %>%
    type_convert()
  
  return(data)
}

join_understat_team_stats <- function(understat_team_stats){
  
  data <-
    understat_team_stats %>%
    filter(data_type=="team_stats") %>%
    select(data) %>%
    unnest(data)
  
  return(data)
}

join_fivethirtyeight <- function(fivethirtyeight){
  
  data <- fivethirtyeight
  
  return(data)
}

join_canpl <- function(canpl){
  
  canpl_tidy <-
    canpl %>%
    mutate(season=as.numeric(str_sub(name,start=-8,end=-5)),.before="name") %>%
    mutate(data_type=as.character(str_sub(name,start=4,end=-9)),.before="name") %>%
    mutate(data=map(data,as_tibble)) %>%
    mutate(data=pmap(list(data,data_type,season),tidy_canpl))
  
  data <- list()
  
  data$team_total <-
    canpl_tidy %>%
    filter(data_type=="TeamTotals") %>%
    select(season,data) %>%
    unnest(data)
  
  data$player_total <-
    canpl_tidy %>%
    filter(data_type=="PlayerTotals") %>%
    select(season,data) %>%
    unnest(data)
  
  data$team_match <-
    canpl_tidy %>%
    filter(data_type=="TeamByGame") %>%
    select(season,data) %>%
    unnest(data)
  
  data$player_match <-
    canpl_tidy %>%
    filter(data_type=="PlayerByGame") %>%
    select(season,data) %>%
    unnest(data)
  
  return(data)
}
