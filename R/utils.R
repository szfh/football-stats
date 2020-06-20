fbref_get_selector <- function(page,seasoncode,stattype,statselector){
  
  selector <- case_when(
    # page=="player" && stattype=="stats" ~ glue("%23standard"),
    page=="player" ~ glue("%23stats_{statselector}"),
    # page=="squad" && stattype=="stats" ~ glue("%23standard_squads"),
    page=="squad" ~ glue("%23stats_{statselector}_squads"),
    page=="schedule" ~ glue("%23sched_ks_{seasoncode}_1"),
    page=="league" ~ glue("%23results{seasoncode}1_overall"),
    page=="leagueha" ~ glue("%23results{seasoncode}1_home_away"),
    TRUE ~ glue()
  )
  return(selector)
}

fbref_get_url <- function(page,seasoncode,stattype,statselector){
  
  url <- case_when(
    page %in% c("player","squad") ~ glue("https://fbref.com/en/comps/9/{seasoncode}/{stattype}/"),
    page=="schedule" ~ glue("https://fbref.com/en/comps/9/{seasoncode}/schedule/"),
    page %in% c("league","leagueha") ~ glue("https://fbref.com/en/comps/9/{seasoncode}/"),
    TRUE ~ glue()
  )
  return(url)
}

fbref_scrape <- function(page_url,content_selector_id){
  url <- glue("http://acciotables.herokuapp.com/?page_url={page_url}&content_selector_id={content_selector_id}")
  print(glue("url: {url}"))
  
  data <-
    url %>%
    read_html() %>%
    html_table(header=FALSE) %>%
    extract2(1)
  
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

make_long_data <- function(data,levels,labels){ # transform data to long format
  data %<>%
    filter_at(levels,any_vars(!is.na(.))) %>%
    pivot_longer(cols=levels,names_to="key",values_to="n") %>%
    mutate(key=factor(key,levels=levels,labels=labels)) %>%
    group_by(key)
  
  return(data)
}

make_long_matches <- function(matches){ # transform matches to long format

  matches %<>%
    pivot_longer(cols=c(home,away),
                 names_to="ha",
                 values_to="team") %>%
    left_join(matches) %>%
    mutate(
      opposition=ifelse(ha=="home",away,home),
      glsf=ifelse(ha=="home",homegls,awaygls),
      glsa=ifelse(ha=="home",awaygls,homegls),
      xgf=ifelse(ha=="home",homexg,awayxg),
      xga=ifelse(ha=="home",awayxg,homexg),
    #   xGF538=ifelse(ha=="home",xGHome538,xGAway538),
    #   xGA538=ifelse(ha=="home",xGAway538,xGHome538),
    #   spiF=ifelse(ha=="Home",spi1,spi2),
    #   spiA=ifelse(ha=="Home",spi2,spi1),
    #   probF=ifelse(ha=="Home",prob1,prob2),
    #   probA=ifelse(ha=="Home",prob2,prob1),
    #   proj_scoreF=ifelse(ha=="Home",proj_score1,proj_score2),
    #   proj_scoreA=ifelse(ha=="Home",proj_score2,proj_score1),
    #   importanceF=ifelse(ha=="Home",importance1,importance2),
    #   importanceA=ifelse(ha=="Home",importance2,importance1),
    #   nsxGF538=ifelse(ha=="Home",nsxGHome538,nsxGAway538),
    #   nsxGA538=ifelse(ha=="Home",nsxGAway538,nsxGHome538),
    #   adj_scoreF=ifelse(ha=="Home",adj_score1,adj_score2),
    #   adj_scoreA=ifelse(ha=="Home",adj_score2,adj_score1)
    )
  
  return(matches)
}

filter_season <- function(data,season="2019-20"){ # filter correct season and teams
  data %<>%
    filter(season %in% !!season)
  
  return(data)
}

# filter correct season and teams
filter_season_team <- function(data,season="2019-20",squad="Southampton"){
  data %<>%
    filter(season %in% !!season) %>%
    filter(squad %in% !!squad)
  
  return(data)
}

filter_na <- function(data,cols){ # filter na
  data %<>%
    filter_at(cols,any_vars(!is.na(.)))
  
  return(data)
}

get_mva <- function(xG,n=6){ # windowed average xG
  
  xGlag <- list()
  xGlag[[1]] <- xG
  
  for(i in 2:n){
    xGlag[[i]] <- lag(xG,(i-1))
  }
  
  mva <- xGlag %>%
    as.data.frame %>%
    rowMeans(na.rm=TRUE)
  
  return(mva)
}

# latest_data <- matches %>%
#   filter(!is.na(GoalsHome)&!is.na(GoalsAway)) %>%
#   summarise(last(Date)) %>%
#   extract2(1)
