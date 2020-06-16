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
  if(page %in% c("squad","player")){
    names(data) <-
      glue("{data[1,]} {data[2,]}") %>%
      str_squish() %>%
      str_to_lower() %>%
      str_replace_all(c(" "="_","%"="pc","#"="n")) %>%
      str_remove_all(c("/","\\(","\\)")) %>%
      make.unique(sep="_") %>%
      print
    
    data %<>% slice(-1,-2)
  }
  if(page %in% "schedule"){
    names(data) <-
      glue("{data[1,]}") %>%
      str_squish() %>%
      str_to_lower() %>%
      str_replace_all(c(" "="_","%"="pc","#"="n")) %>%
      str_remove_all(c("/","\\(","\\)")) %>%
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

fbref_tidy <- function(data,page,stattype){ #all data editing, selecting, renaming in here?
  
  if(page %in% c("squad","player","schedule")){
    data %<>%
      select(-any_of(c("rk","matches","notes","match_report"))) %>%
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

fbref_get_selector <- function(page,seasoncode,stattype,statselector){
  
  selector <- case_when(
    # page=="player" && stattype=="stats" ~ glue("%23standard"),
    page=="player" ~ glue("%23stats_{statselector}"),
    # page=="squad" && stattype=="stats" ~ glue("%23standard_squads"),
    page=="squad" ~ glue("%23stats_{statselector}_squads"),
    page=="schedule" ~ glue("%23sched_ks_{seasoncode}_1"),
    TRUE ~ glue()
  )
  return(selector)
}

fbref_get_url <- function(page,seasoncode,stattype,statselector){
  
  url <- case_when(
    page %in% c("player","squad") ~ glue("https://fbref.com/en/comps/9/{seasoncode}/{stattype}/"),
    page=="schedule" ~ glue("https://fbref.com/en/comps/9/{seasoncode}/schedule/"),
    TRUE ~ glue()
  )
  return(url)
}

# scrape fbref old
fbref_scrape_old <- function(url,comment=FALSE,fix_columns=FALSE,extract=NA){
  
  Sys.sleep(0.1)
  
  if(comment==TRUE){ # table is inside html comment
    data_table <-
      read_html(url) %>%
      html_nodes(xpath="//comment()") %>%
      html_text() %>%
      paste(collapse="") %>%
      read_html() %>%
      html_nodes("table") %>%
      html_table
  } else { # table not inside html comment
    data_table <-
      read_html(url) %>%
      html_nodes("table") %>%
      html_table()
  }
  
  if(is.na(extract)==FALSE){ # select table
    data_table <- data_table %>%
      extract2(extract)
  }
  
  if(fix_columns==TRUE){
    names(data_table) <- data_table[1,] # move first row to names
    data_table <- as_tibble(data_table, .name_repair="unique") # create tibble
    data_table <- data_table %>% slice(-1) # remove first row
  } else {
    data_table <- as_tibble(data_table, .name_repair = "unique") # create tibble
  }
  
  if("Player" %in% names(data_table)){ # remove duplicated column names from player table
    data_table <- data_table %>%
      filter(Player != "Player")
  }
  
  data_table <- type_convert(data_table) # reset data types
  
  return(data_table)
}

# transform data to long format
make_long_data <- function(data,levels,labels){
  
  data %<>%
    filter_at(levels,any_vars(!is.na(.))) %>%
    pivot_longer(cols=levels,names_to="key",values_to="n") %>%
    mutate(key=factor(key,levels=levels,labels=labels)) %>%
    group_by(key)
  
  return(data)
}

make_long_matches <- function(matches){
  # browser()
  
  # matches %<>%
  #   pivot_longer(cols=c(Home,Away),
  #                names_to="HA",
  #                values_to="Team") %>%
  #   left_join(matches) %>%
  #   mutate(
  #     Opposition=ifelse(HA=="Home",Away,Home),
  #     GoalsF=ifelse(HA=="Home",GoalsHome,GoalsAway),
  #     GoalsA=ifelse(HA=="Home",GoalsAway,GoalsHome),
  #     xGFfbref=ifelse(HA=="Home",xGHomefbref,xGAwayfbref),
  #     xGAfbref=ifelse(HA=="Home",xGAwayfbref,xGHomefbref),
  #     xGF538=ifelse(HA=="Home",xGHome538,xGAway538),
  #     xGA538=ifelse(HA=="Home",xGAway538,xGHome538),
  #     spiF=ifelse(HA=="Home",spi1,spi2),
  #     spiA=ifelse(HA=="Home",spi2,spi1),
  #     probF=ifelse(HA=="Home",prob1,prob2),
  #     probA=ifelse(HA=="Home",prob2,prob1),
  #     proj_scoreF=ifelse(HA=="Home",proj_score1,proj_score2),
  #     proj_scoreA=ifelse(HA=="Home",proj_score2,proj_score1),
  #     importanceF=ifelse(HA=="Home",importance1,importance2),
  #     importanceA=ifelse(HA=="Home",importance2,importance1),
  #     nsxGF538=ifelse(HA=="Home",nsxGHome538,nsxGAway538),
  #     nsxGA538=ifelse(HA=="Home",nsxGAway538,nsxGHome538),
  #     adj_scoreF=ifelse(HA=="Home",adj_score1,adj_score2),
  #     adj_scoreA=ifelse(HA=="Home",adj_score2,adj_score1)
  #   ) %>%
  #   print
  
  return(matches)
}

# filter correct season and teams
filter_season <- function(data,season="2019-20"){
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

# filter na
filter_na <- function(data,cols){
  data %<>%
    filter_at(cols,any_vars(!is.na(.)))
  
  return(data)
}

# windowed average xG
get_mva <- function(xG,n=6){
  
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
