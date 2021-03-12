join_wfr <- function(
  save_path_fbref=here("data","fbref.rds")
){
  fbref_join <- possibly(join_fbref, otherwise=NA)(readRDS(save_path_fbref))
  # data <- list(fbref=fbref_join,understat=understat_join,canpl=canpl_join)
  data <- list(fbref=fbref_join)
  
  return(data)
}

join_fbref <- function(fbref){
  
  fbref_tidy <-
    fbref %>%
    mutate(data=pmap(list(data,data_type,stat),tidy_fbref))
  
  data <- list()
  
  data$table <-
    fbref_tidy %>%
    filter(stat=="league_table") %>%
    select(season,data) %>%
    unnest(cols=data) %>%
    glimpse
  
  data$table_home_away <-
    fbref_tidy %>%
    filter(stat=="league_table_home_away") %>%
    select(season,data) %>%
    unnest(cols=data) %>%
    glimpse
  
  data$season_stat <-
    fbref_tidy %>%
    filter(data_type=="season_stat") %>%
    filter(stat!="league_table") %>%
    filter(stat!="league_table_home_away") %>%
    select(-data_type,-stat) %$%
    data %>%
    reduce(full_join) %>%
    glimpse
  
  data$matches <-
    fbref_tidy %>%
    filter(data_type=="match_result") %>%
    select(-stat) %>%
    unnest(cols=data) %>%
    glimpse

  data$team_advanced_stats_match <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats") %>%
    filter(team_or_player=="team") %>%
    unnest(data) %>%
    group_by(stat) %>%
    nest() %>%
    mutate(data=map(data,remove_empty,which="cols")) %>%
    pull(data) %>%
    reduce(full_join) %>%
    glimpse
  
  data$player_advanced_stats_match <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats") %>%
    filter(team_or_player=="player") %>%
    unnest(data) %>%
    group_by(stat) %>%
    nest() %>%
    mutate(data=map(data,remove_empty,which="cols")) %>%
    pull(data) %>%
    reduce(full_join) %>%
    glimpse
  
  return(data)
}

tidy_fbref <- function(data,data_type=NA,stat=NA){
  if(data_type=="match_url"){
    data <-
      data %>%
      as_tibble() %>%
      rename("url"="value")
  }
  if(data_type=="match_result"){
    data <-
      data %>%
      as_tibble() %>%
      select(-contains(c("Attendance","Venue","Referee","Notes")))
  }
  if(data_type=="season_stat" && stat %in% c("league_table","league_table_home_away")){
    data <-
      data %>%
      as_tibble() %>%
      select(-contains(c("90","Last.5","Top.Team.Scorer","Goalkeeper","Notes")))
  }
  if(data_type=="season_stat" && stat %in% c("keeper","keeper_adv")){
    data <-
      data %>%
      as_tibble() %>%
      # select(-any_of(c("Num_Players":"Min_Playing"))) %>%
      select(-contains(c("Players","Playing","90","Last.5","Top.Team.Scorer","Goalkeeper","Notes")))
  }
  if(data_type=="season_stat"){
    data <-
      data %>%
      as_tibble()
  }
  
  if(data_type=="advanced_stats" && stat=="keeper"){
    # browser()
    data <-
      data %>%
      as_tibble() %>%
      select(-contains(c("Player","Nation","Age","Min","Att_Passes")))
  }
  if(data_type=="advanced_stats"){
    # browser()
    data <-
      data %>%
      as_tibble() #%>%
    # select(-contains(c("Min")))
  }
  
  return(data)
}
