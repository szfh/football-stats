join_wfr <- function(
  save_path_fbref=here("data","fbref.rds")
){
  fbref_join <- possibly(join_fbref, otherwise=NA)(readRDS(save_path_fbref))
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
    select(data) %>%
    unnest(data)
  
  data$table_home_away <-
    fbref_tidy %>%
    filter(stat=="league_table_home_away") %>%
    select(data) %>%
    unnest(data)
  
  data$season_stat <-
    fbref_tidy %>%
    filter(data_type=="season_stat") %>%
    filter(stat!="league_table") %>%
    filter(stat!="league_table_home_away") %>%
    pull(data) %>%
    reduce(full_join)
  
  data$matches <-
    fbref_tidy %>%
    filter(data_type=="match_result") %>%
    select(data) %>%
    unnest(data)
  
  data$team_advanced_stats_match <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats") %>%
    filter(team_or_player=="team") %>%
    select(stat,data) %>%
    unnest(data) %>%
    group_by(stat) %>%
    nest() %>%
    ungroup() %>%
    mutate(data=map(data,remove_empty,which="cols")) %>%
    filter(stat!="misc") %>%
    pull(data) %>%
    reduce(full_join)
  
  data$player_advanced_stats_match <-
    fbref_tidy %>%
    filter(data_type=="advanced_stats") %>%
    filter(team_or_player=="player") %>%
    select(stat,data) %>%
    unnest(data) %>%
    group_by(stat) %>%
    nest() %>%
    ungroup() %>%
    mutate(data=map(data,remove_empty,which="cols")) %>%
    filter(stat!="misc") %>%
    pull(data) %>%
    reduce(full_join)
  
  # to do: match summaries
  
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
  else if(data_type=="season_stat" && stat %in% c("keeper","keeper_adv")){
    data <-
      data %>%
      as_tibble() %>%
      select(-contains(c("Players","Playing","90","Last.5","Top.Team.Scorer","Goalkeeper","Notes")))
  }
  else if(data_type=="season_stat"){
    data <-
      data %>%
      as_tibble()
  }
  
  if(data_type=="advanced_stats" && stat=="keeper"){
    data <-
      data %>%
      as_tibble() %>%
      select(-contains(c("Player","Nation","Age","Min","Att_Passes")))
  }
  else if(data_type=="advanced_stats"){
    data <-
      data %>%
      as_tibble()
  }
  
  return(data)
}

