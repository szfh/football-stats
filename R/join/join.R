join <- function(){
  fbref <- readRDS(file=here("data","fbref-raw.rds"))
  understat <- readRDS(file=here("data","understat-raw.rds"))
  canpl <- readRDS(file=here("data","canpl-raw.rds"))
  
  data <- list()
  
  # tidy  
  fbref <- fbref %>%
    mutate(data=pmap(list(data,page,stattype), possibly(fbref_tidy, otherwise=NA))) %>%
    select(-any_of(c("statselector","seasoncode","page_url","content_selector_id")))
  
  # understat <- understat
  # mutate(data=pmap(function_here)) %>% # make understat_tidy?
  # select() # delete non-required columns
  
  # canpl <- canpl
  
  # join
  data$table <-
    fbref %>%
    filter(page=="league") %>%
    select(-page,-stattype) %>%
    unnest(cols=data)
  
  data$squad <-
    fbref %>%
    filter(page=="squad") %>%
    select(-page) %>%
    unnest(cols=data) %>%
    group_by(stattype) %>%
    nest() %>%
    mutate(data=map(data,remove_empty,which="cols")) %$%
    data %>%
    reduce(full_join)
  
  data$squad <-
    data$table %>%
    left_join(data$squad)
  
  data$players <-
    fbref %>%
    filter(page=="player") %>%
    select(-page) %>%
    unnest(cols=data) %>%
    group_by(stattype) %>%
    nest() %>%
    mutate(data=map(data,remove_empty,which="cols")) %$%
    data %>%
    reduce(full_join)
  
  data$matches <-
    fbref %>%
    filter(page=="schedule") %>%
    select(-page,-stattype) %>%
    unnest(cols=data)
  
  data$shots <-
    understat %>%
    filter(datatype=="shots") %>%
    select(data) %>%
    unnest(data)
  
  data$match_stats <-
    understat %>%
    filter(datatype=="stats") %>%
    select(data) %>%
    unnest(data)
  
  data$us_schedule <- 
    understat %>%
    filter(stattype=="schedule") %>%
    select(-id,-isResult) %>%
    unnest(cols="data") %>%
    mutate(match_id=id) %>%
    type_convert()
  
  data$canpl <- canpl
  
  return(data)
}

fbref_clean_names <- function(data,page){
  if(page %in% c("squad","player","leagueha")){
    names(data) <-
      glue("{data[1,]} {data[2,]}") %>%
      str_squish() %>%
      str_to_lower() %>%
      str_replace_all(c(" "="_","%"="pc","#"="n")) %>%
      str_remove_all("[/ \\( \\)]") %>%
      make.unique(sep="_") %>%
      print
    
    data %<>% slice(-1,-2)
  }
  if(page %in% c("schedule","league")){
    names(data) <-
      glue("{data[1,]}") %>%
      str_squish() %>%
      str_to_lower() %>%
      str_replace_all(c(" "="_","%"="pc","#"="n")) %>%
      str_remove_all("[/ \\( \\)]") %>%
      make.unique(sep="_") %>%
      print
    
    data %<>% slice(-1)
  }
  if("player" %in% names(data)){ # remove duplicated column names from player table
    data %<>% filter(player != "Player")
  }
  if("wk" %in% names(data)){ # remove duplicated column names + blank rows from schedule
    data %<>% filter(wk != "Wk") %>% filter(wk != "")
  }
  
  data %<>% type_convert # refactor data types
  
  return(data)
}

fbref_tidy <- function(data,page,stattype){
  
  if(page %in% c("squad","player","schedule","league","leagueha")){
    data %<>%
      select(-any_of(c("rk","matches","notes","match_report","top_team_scorer","goalkeeper"))) %>%
      select(-contains(c("pc","90")))
  }
  if(page=="player"){
    data %<>%
      separate("nation",c(NA,"nation"),sep=" ",fill="right") %>%
      separate("pos",c("pos1",NA,"pos2"),sep=c(2,3),fill="right")
  }
  if(page=="schedule"){
    data %<>%
      separate("score",c("homegls","awaygls"),sep="[:punct:]",fill="right") %>%
      rename("homexg"="xg","awayxg"="xg_1")
  }
  if(stattype %in% c("keepers","keepersadv")){
    data %<>%
      rename("n_pl_gk"=any_of("n_pl")) %>%
      select(-any_of(c("playing_time_starts","playing_time_mp","playing_time_min")))
  }
  return(data)
}
