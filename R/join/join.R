source(here("R","join","tidy.R"),encoding="utf-8")

join <- function(
  save_path_fbref=here("data","fbref.rds"),
  save_path_fbref_urls=here("data","fbref_urls.rds"),
  save_path_understat=here("data","understat.rds"),
  save_path_fivethirtyeight=here("data","fivethirtyeight.rds"),
  save_path_canpl=here("data","canpl.rds")
){
  fbref_join <- possibly(join_fbref, otherwise=NA)(readRDS(save_path_fbref))
  fbref_urls_join <- possibly(join_fbref_urls, otherwise=NA)(readRDS(save_path_fbref_urls))
  understat_join <- possibly(join_understat, otherwise=NA)(readRDS(save_path_understat))
  fivethirtyeight_join <- possibly(join_fivethirtyeight, otherwise=NA)(readRDS(save_path_fivethirtyeight))
  canpl_join <- possibly(join_canpl, otherwise=NA)(readRDS(save_path_canpl))
  data <- list(
    fbref=fbref_join,
    fbref_urls=fbref_urls_join,
    understat=understat_join,
    fivethirtyeight=fivethirtyeight_join,
    canpl=canpl_join)
  
  return(data)
}

join_fbref <- function(fbref){
  fbref_tidy <-
    fbref %>%
    mutate(data=map(data,as_tibble)) %>%
    mutate(data=pmap(list(data,data_type,stat,team_or_player),tidy_fbref))
  
  data <- list()
  
  data$match_results <-
    fbref_tidy %>%
    filter(data_type=="match_result") %>%
    select(data) %>%
    unnest(data)
  
  data$match_results_allcomp <-
    fbref_tidy %>%
    filter(data_type=="match_result_allcomp") %>%
    select(Season=season,data) %>%
    unnest(data)
  
  data$match_summary <-
    fbref_tidy %>%
    filter(data_type=="match_summary") %>%
    select(data) %>%
    unnest(data)
  
  data$match_lineups <-
    fbref_tidy %>%
    filter(data_type=="match_lineups") %>%
    select(data) %>%
    unnest(data)
  
  data$match_shots <-
    fbref_tidy %>%
    filter(data_type=="match_shots") %>%
    select(data) %>%
    unnest(data)
  
  data$table <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="league_table") %>%
    select(data) %>%
    unnest(data) %>%
    select(-Team_or_Opponent)
  
  data$table_home_away <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="league_table_home_away") %>%
    select(data) %>%
    unnest(data)
  
  data$season_stat_defense <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="defense") %>%
    select(data) %>%
    unnest(data)
  
  data$season_stat_gca <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="goal_shot_creation") %>%
    select(data) %>%
    unnest(data)
  
  data$season_stat_keeper <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="keeper") %>%
    select(data) %>%
    unnest(data)
  
  data$season_stat_keeper_adv <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="keeper_adv") %>%
    select(data) %>%
    unnest(data)
  
  data$season_stat_misc <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="misc") %>%
    select(data) %>%
    unnest(data)
  
  data$season_stat_passing <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="passing") %>%
    select(data) %>%
    unnest(data)
  
  data$season_stat_passing_types <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="passing_types") %>%
    select(data) %>%
    unnest(data)
  
  data$season_stat_possession <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="possession") %>%
    select(data) %>%
    unnest(data)
  
  data$season_stat_playing_time <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="playing_time") %>%
    select(data) %>%
    unnest(data)
  
  data$season_stat_shooting <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="shooting") %>%
    select(data) %>%
    unnest(data)
  
  data$season_stat_standard <-
    fbref_tidy %>%
    filter(data_type=="season_stat" & stat=="standard") %>%
    select(data) %>%
    unnest(data)
  
  data$advanced_stats_player_possession <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="possession" & team_or_player=="player") %>%
    select(data) %>%
    unnest(data)
  
  data$advanced_stats_player_passing <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="passing" & team_or_player=="player") %>%
    select(data) %>%
    unnest(data)
  
  data$advanced_stats_player_summary <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="summary" & team_or_player=="player") %>%
    select(data) %>%
    unnest(data)
  
  data$advanced_stats_player_defense <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="defense" & team_or_player=="player") %>%
    select(data) %>%
    unnest(data)
  
  data$advanced_stats_player_misc <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="misc" & team_or_player=="player") %>%
    select(data) %>%
    unnest(data)
  
  data$advanced_stats_player_passing_types <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="passing_types" & team_or_player=="player") %>%
    select(data) %>%
    unnest(data)
  
  data$advanced_stats_player_keeper <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="keeper" & team_or_player=="player") %>%
    select(data) %>%
    unnest(data)
  
  data$advanced_stats_team_possession <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="possession" & team_or_player=="team") %>%
    select(data) %>%
    unnest(data)
  
  data$advanced_stats_team_passing <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="passing" & team_or_player=="team") %>%
    select(data) %>%
    unnest(data)
  
  data$advanced_stats_team_summary <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="summary" & team_or_player=="team") %>%
    select(data) %>%
    unnest(data)
  
  data$advanced_stats_team_defense <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="defense" & team_or_player=="team") %>%
    select(data) %>%
    unnest(data)
  
  data$advanced_stats_team_misc <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="misc" & team_or_player=="team") %>%
    select(data) %>%
    unnest(data)
  
  data$advanced_stats_team_passing_types <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="passing_types" & team_or_player=="team") %>%
    select(data) %>%
    unnest(data)
  
  data$advanced_stats_team_keeper <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats" & stat=="keeper" & team_or_player=="team") %>%
    select(data) %>%
    unnest(data)
  
  return(data)
}

join_fbref_urls <- function(fbref_urls){
  data <-
    fbref_urls
  
  return(data)
}

join_understat <- function(understat){
  understat_tidy <-
    understat #%>%
  # mutate(data=map(data,tidy_understat))
  
  data <- list()
  
  data$results <-
    understat_tidy %>%
    filter(data_type=="results") %>%
    select(data) %>%
    unnest(data)
  
  data$shots <-
    understat_tidy %>%
    filter(data_type=="shots") %>%
    select(data) %>%
    unnest(data)
  
  return(data)
}

join_fivethirtyeight <- function(fivethirtyeight){
  fivethirtyeight_tidy <-
    fivethirtyeight
  
  data <- fivethirtyeight_tidy
  
  return(data)
}

join_canpl <- function(canpl){
  
  canpl_tidy <-
    canpl %>%
    mutate(Season=as.numeric(str_sub(name,start=-8,end=-5)),.before="name") %>%
    mutate(Data_Type=as.character(str_sub(name,start=4,end=-9)),.before="name") %>%
    mutate(data=map(data,as_tibble)) %>%
    mutate(data=map(data,tidy_canpl))
  
  data <- list()
  
  data$team_total <-
    canpl_tidy %>%
    filter(Data_Type=="TeamTotals") %>%
    select(Season,data) %>%
    unnest(data)
  
  data$player_total <-
    canpl_tidy %>%
    filter(Data_Type=="PlayerTotals") %>%
    select(Season,data) %>%
    unnest(data)
  
  data$team_match <-
    canpl_tidy %>%
    filter(Data_Type=="TeamByGame") %>%
    select(Season,data) %>%
    unnest(data)
  
  data$player_match <-
    canpl_tidy %>%
    filter(Data_Type=="PlayerByGame") %>%
    select(Season,data) %>%
    unnest(data)
  
  return(data)
}
