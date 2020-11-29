join <- function(
  fbref=readRDS(file=here("data","fbref.rds")),
  understat=readRDS(file=here("data","understat.rds")),
  canpl=readRDS(file=here("data","canpl.rds"))
){
  
  browser()
  
  # fbref_join <- join_fbref(fbref)
  fbref_join <- possibly(join_fbref, otherwise=NA)(fbref)
  # understat_join <- join_understat(understat)
  understat_join <- possibly(join_understat, otherwise=NA)(understat)
  # canpl_join <- join_canpl(canpl)
  canpl_join <- possibly(join_fbref, otherwise=NA)(canpl)
  # possibly(join_understat(understat),otherwise=NA)
  # possibly(join_canpl(canpl),otherwise=NA)
  
  data <- list(fbref=fbref_join,understat=understat_join,canpl=canpl_join)
  
  browser()
  
  return(data)
  
}

join_fbref <- function(fbref){
  
  # browser()
  
  data <- list()
  
  fbref <-
    fbref %>%
    mutate(data=pmap(list(data,page,stattype), possibly(fbref_tidy, otherwise=NA))) %>%
    select(-any_of(c("statselector","seasoncode","page_url","content_selector_id")))
  
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
    reduce(full_join) %>%
    mutate(vs=str_detect(squad,"vs "))
  
  data$squad <-
    data$table %>%
    full_join(data$squad)
  
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
   
  # browser()
  
  return(data)
}

join_understat <- function(understat){
  
  data <- list()
  
  understat <-
    understat %>%
    # mutate(data=pmap(function_here)) %>% # make understat_tidy?
    select(-any_of(c("statselector")))
  
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
    mutate(match_id=id)
  
  return(data)
}

join_canpl <- function(canpl){
  
  data <- list()
  
  canpl <-
    canpl %>%
    select(-any_of(c("path")))
  
  data$canpl <-
    canpl
  
  return(data)
}

join2 <- function(){
  fbref <- readRDS(file=here("data","fbref.rds"))
  understat <- readRDS(file=here("data","understat.rds"))
  canpl <- readRDS(file=here("data","canpl.rds"))
  
  data <- list()
  
  # tidy  
  fbref <-
    fbref %>%
    mutate(data=pmap(list(data,page,stattype), possibly(fbref_tidy, otherwise=NA))) %>%
    select(-any_of(c("statselector","seasoncode","page_url","content_selector_id")))
  
  understat <-
    understat %>%
    # mutate(data=pmap(function_here)) %>% # make understat_tidy?
    select(-any_of(c("statselector")))
  
  canpl <-
    canpl %>%
    select(-any_of(c("path")))
  
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
    reduce(full_join) %>%
    mutate(vs=str_detect(squad,"vs "))
  
  data$squad <-
    data$table %>%
    full_join(data$squad)
  
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
    mutate(match_id=id)
  
  data$canpl <-
    canpl
  
  #type convert
  # data <-
  #   data %>%
  #   map(type_convert)
  
  return(data)
}

fbref_tidy <- function(data,page,stattype){
  
  if(page %in% c("squad","player","schedule","league","leagueha")){
    data <-
      data %>%
      select(-any_of(c("rk","matches","notes","match_report","top_team_scorer","goalkeeper"))) %>%
      select(-contains(c("pc","90"))) %>%
      mutate(across(any_of("age"),as.character))
  }
  if(page=="player"){
    data <-
      data %>%
      separate("nation",c(NA,"nation"),sep=" ",fill="right") %>%
      separate("pos",c("pos1",NA,"pos2"),sep=c(2,3),fill="right")
  }
  if(page=="schedule"){
    data <-
      data %>%
      separate("score",c("homegls","awaygls"),sep="[:punct:]",fill="right") %>%
      rename("homexg"="xg","awayxg"="xg_1")
  }
  if(stattype %in% c("keepers","keepersadv")){
    data <-
      data %>%
      rename("n_pl_gk"=any_of("n_pl")) %>%
      select(-any_of(c("playing_time_starts","playing_time_mp","playing_time_min")))
  }
  return(data)
}
