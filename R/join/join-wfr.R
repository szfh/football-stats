join_wfr <- function(
  save_path_fbref=here("data","fbref.rds")
){
  fbref_join <- possibly(join_fbref, otherwise=NA)(readRDS(save_path_fbref))
  # data <- list(fbref=fbref_join,understat=understat_join,canpl=canpl_join)
  data <- list(fbref=fbref_join)
  
  return(data)
}

join_fbref <- function(fbref){
  data <- list()
  
  glimpse(fbref)
  
  browser()
  
  fbref_tidy <-
    fbref %>%
    mutate(data=pmap(list(data,data_type,stat),tidy_fbref)) %>%
    # select(-any_of(c("stat_key","season_key","page_url","content_selector_id"))) %>%
    glimpse
  
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
  
  browser()
  
  # data$squad <-
  #   fbref %>%
  #   filter(page=="squad") %>%
  #   select(-page) %>%
  #   unnest(cols=data) %>%
  #   group_by(stat) %>%
  #   nest() %>%
  #   mutate(data=map(data,remove_empty,which="cols")) %$%
  #   data %>%
  #   reduce(full_join) %>%
  #   mutate(vs=str_detect(squad,"vs "))
  # 
  # data$squad <-
  #   data$table %>%
  #   full_join(data$squad)
  # 
  # data$players <-
  #   fbref %>%
  #   filter(page=="player") %>%
  #   select(-page) %>%
  #   unnest(cols=data) %>%
  #   group_by(stat) %>%
  #   nest() %>%
  #   mutate(data=map(data,remove_empty,which="cols")) %$%
  #   data %>%
  #   reduce(full_join)
  # 
  # data$matches <-
  #   fbref %>%
  #   filter(page=="schedule") %>%
  #   select(-page,-stat,-home,-away) %>%
  #   unnest(cols=data)
  # 
  # data$events <-
  #   fbref %>%
  #   filter(stat=="events") %>%
  #   select(-page) %>%
  #   unnest(cols=data)
  # 
  # data$shots <-
  #   fbref %>%
  #   filter(stat=="shots") %>%
  #   select(-page) %>%
  #   unnest(cols=data)
  
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
      as_tibble()
  }
  if(stat %in% c("league_table","league_table_home_away")){
    data <-
      data %>%
      as_tibble() %>%
      select(-contains(c("90","Last.5","Top.Team.Scorer","Goalkeeper","Notes")))
  }
  else if(data_type=="season_stat"){
    data <-
      data %>%
      as_tibble()
  }
  
  return(data)
}
