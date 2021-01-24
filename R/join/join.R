join <- function(
  fbref=readRDS(file=here("data","fbref.rds")),
  understat=readRDS(file=here("data","understat.rds")),
  canpl=readRDS(file=here("data","canpl.rds"))
){
  fbref_join <- possibly(join_fbref, otherwise=NA)(fbref)
  understat_join <- possibly(join_understat, otherwise=NA)(understat)
  canpl_join <- possibly(join_fbref, otherwise=NA)(canpl)
  
  data <- list(fbref=fbref_join,understat=understat_join,canpl=canpl_join)
  
  return(data)
}

join_fbref <- function(fbref){
  data <- list()

  fbref <-
    fbref %>%
    mutate(data=pmap(list(data,page,stat), possibly(fbref_tidy, otherwise=NA))) %>%
    select(-any_of(c("stat_key","season_key","page_url","content_selector_id")))

  data$table <-
    fbref %>%
    filter(page=="league") %>%
    select(-page,-stat) %>%
    unnest(cols=data)
  
  data$squad <-
    fbref %>%
    filter(page=="squad") %>%
    select(-page) %>%
    unnest(cols=data) %>%
    group_by(stat) %>%
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
    group_by(stat) %>%
    nest() %>%
    mutate(data=map(data,remove_empty,which="cols")) %$%
    data %>%
    reduce(full_join)
  
  data$matches <-
    fbref %>%
    filter(page=="schedule") %>%
    select(-page,-stat,-home,-away) %>%
    unnest(cols=data)

  data$events <-
    fbref %>%
    filter(stat=="events") %>%
    select(-page) %>%
    unnest(cols=data)
  
  data$shots <-
    fbref %>%
    filter(stat=="shots") %>%
    select(-page) %>%
    unnest(cols=data)

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

fbref_tidy <- function(data,page,stat){
  if(page %in% c("squad","player","schedule","league","leagueha")){
    data <-
      data %>%
      select(-any_of(c("rk","matches","notes","match_report","top_team_scorer","goalkeeper"))) %>%
      select(-contains(c("pc","90"))) %>%
      mutate(across(any_of("age"),as.character))
  }
  if(page %in% "player"){
    data <-
      data %>%
      separate("nation",c(NA,"nation"),sep=" ",fill="right") %>%
      separate("pos",c("pos1",NA,"pos2"),sep=c(2,3),fill="right")
  }
  if(page %in% "schedule"){
    data <-
      data %>%
      separate("score",c("homegls","awaygls"),sep="[:punct:]",fill="right") %>%
      rename("homexg"="xg","awayxg"="xg_1")
  }
  if(stat %in% c("keepers","keepersadv")){
    data <-
      data %>%
      rename("n_pl_gk"=any_of("n_pl")) %>%
      select(-any_of(c("playing_time_starts","playing_time_mp","playing_time_min")))
  }
  if(stat %in% "events"){
    data <-
      data %>%
      mutate(event=str_remove_all(event,"\n")) %>%
      mutate(event=str_remove_all(event,"\t")) %>%
      mutate(event=str_squish(event)) %>%
      mutate(
        team=case_when(
          type=="event" ~ "event",
          type=="event a" ~ "Home",
          type=="event b" ~ "Away",
          TRUE ~ type)) %>%
      select(-type) %>%
      filter(team!="event") %>%
      separate(event,c("time","desc"),sep="&rsquor;",extra="merge",fill="right") %>%
      mutate(half=case_when(
        as.numeric(str_sub(time,1,2)) <= 45 ~ 1,
        TRUE ~ 2 # extra time?
      )) %>%
      mutate(time=case_when(
        str_detect(time,"\\+") ~ as.character(as.numeric(str_sub(time,1,2))+as.numeric(str_sub(time,3))),
        TRUE ~ time
      )) %>%
      separate(desc,c("homegls",NA,"awaygls","desc"),sep=c(1,2,3),extra="merge",fill="right") %>%
      separate(desc,c("desc","type"),sep=" — ",extra="merge",fill="right") %>%
      mutate(type=case_when(
        str_detect(type,"Goal") ~ "Goal",
        str_detect(desc,"Penalty Kick") ~ "Goal (pen)",
        str_detect(desc,"Penalty Miss") ~ "Miss (pen)",
        TRUE ~ type
      )) %>%
      mutate(state=as.numeric(homegls)-as.numeric(awaygls)) %>%
      mutate(desc=str_remove_all(desc,coll("Penalty Kick — Substitute| —|Penalty Miss"))) %>%
      separate(desc,c("player1","player2"),sep=coll("for |Assist:|Penalty Kick —|Penalty Kick|Penalty saved by |Penalty Miss")) %>%
      relocate(half,time,type,team,player1,player2,homegls,awaygls,state) #%>%
      # type_convert()
  }
  if(stat %in% "shots"){
    data <-
      data %>%
      mutate(half=case_when(
        as.numeric(str_sub(minute,1,2)) <= 45 ~ 1,
        TRUE ~ 2 #extra time?
      )) %>%
      mutate(minute=case_when(
        str_detect(as.character(minute),"\\+") ~ (as.numeric(str_sub(minute,1,2))+as.numeric(str_sub(minute,3))),
        TRUE ~ as.numeric(minute)
      ))
  }
  
  return(data)
}
