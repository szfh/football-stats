tidy_fbref <- function(data,data_type=NA,stat=NA,team_or_player=NA){
  if(data_type=="match_url"){
    data <-
      data %>%
      rename("url"="value")
  }
  if(data_type=="match_result"){
    data <-
      data %>%
      select(-contains(c("Attendance","Venue","Referee","Notes")))
  }
  if(data_type=="season_stat" && stat %in% c("league_table","league_table_home_away")){
    data <-
      data %>%
      select(-contains(c("90","Last.5","Top.Team.Scorer","Goalkeeper","Notes")))
  }
  if(data_type=="season_stat" && stat %in% c("keeper","keeper_adv")){
    data <-
      data %>%
      select(-contains(c("Players","Playing","90","Last.5","Top.Team.Scorer","Goalkeeper","Notes")))
  }
  if(data_type=="advanced_stats" && stat=="keeper"){
    data <-
      data %>%
      select(-contains("percent")) %>%
      select(-c(Home_Score:Home_xG,Away_Score:Away_xG)) %>%
      select(-contains(c("Player","Nation","Age","Min","Att_Passes")))
  }
  if(data_type=="advanced_stats" && stat=="keeper" && team_or_player=="team" && dim(data)[1]>2){
    data <-
      data %>%
      group_by(across(where(is.character))) %>%
      summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop")
  }
  
  return(data)
}

tidy_understat <- function(data){
  data <-
    data
  
  return(data)
}

tidy_canpl <- function(data){
  data <-
    data %>%
    select(-contains(c("Season")))
  
  return(data)
}
