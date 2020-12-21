scrape_fbref <- function(save_path=here("data","fbref.rds"),current_season="2020-21"){
  
  fbref_saved <- readRDS(save_path)
  
  eplseasons <- tribble(~season, ~seasoncode, #advanced/non-advanced? # rename seasoncode to code/pagecode
                        "2020-21",10728,
                        "2019-20",3232,
                        "2018-19",1889,
                        "2017-18",1631,
                        # "2016-17",1526,
  )
  
  data_types_squad_player <- tribble(~page,
                                     "player",
                                     "squad",
  )
  
  tables_squad_player <- tribble(~stattype, ~statselector,
                                 "stats","standard",
                                 "keepers","keeper",
                                 "keepersadv","keeper_adv",
                                 "shooting","shooting",
                                 "passing","passing",
                                 "passing_types","passing_types",
                                 "gca","gca",
                                 "defense","defense",
                                 "possession","possession",
                                 "playingtime","playing_time",
                                 "misc","misc",
  )
  
  data_types_league <- tribble(~page,
                               "schedule",
                               "league",
                               "leagueha",
  )
  
  fbref_squad_player_all <- #fbref$squad$all / fbref$player$all
    tibble() %>% # all squad + player data parameters
    bind_rows(crossing(data_types_squad_player,tables_squad_player)) %>% # (squad + player) * datatypes
    bind_rows(data_types_league) %>% # fixtures
    crossing(eplseasons)
  
  fbref_squad_player_keep <- #fbref$squad$keep / fbref$player$keep
    fbref_saved %>% # remove data to be scraped from saved
    filter(page %in% c("player","squad","league","leagueha","schedule")) %>%
    filter(season!=current_season)
  ###
  # add extra data to be manually removed here
  ###
  
  fbref_squad_player_new <- #fbref$squad$new / fbref$player$new
    anti_join(fbref_squad_player_all, fbref_squad_player_keep) %>%
    mutate(page_url=fbref_get_url(page,seasoncode,stattype,statselector)) %>%
    mutate(content_selector_id=fbref_get_selector(page,seasoncode,stattype,statselector)) %>%
    mutate(data=pmap(list(page_url, content_selector_id, page, stattype), possibly(fbref_scrape, otherwise=NA)))
  
  fbref_squad_player <-
    bind_rows(fbref_squad_player_keep,fbref_squad_player_new) %>%
    filter(!is.na(data))
  
  fbref_matches_all <- # derive all matches from fbref data frame
    fbref_saved %>%
    filter(page=="schedule") %>%
    select(page,stattype,statselector,season,seasoncode,page_url,content_selector_id,data) %>% #matchcode
    unnest(cols=data) %>%
    select(page:seasoncode,home,away,matchcode=code) %>%
    mutate(page="match",stattype="events") %>% # crossing?
    filter(!is.na(matchcode)) %>%
    filter(season=="2020-21") # delete later
  
  fbref_shots_keep <-
    fbref_saved %>%
    filter(stattype=="shots") %>%
    filter(!is.na(data)) %>%
    filter(!is.null(data))
  
  fbref_shots_new <-
    anti_join(fbref_matches_all, fbref_shots_keep, by="matchcode") %>%
    mutate(stattype="shots") %>%
    mutate(page_url=fbref_get_url(page,matchcode=matchcode)) %>%
    mutate(content_selector_id=fbref_get_selector(page,stattype=stattype,matchcode=matchcode)) %>%
    mutate(data=pmap(list(page_url,content_selector_id,page,stattype), possibly(fbref_scrape, otherwise=NA)))
  
  fbref_shots <-
    bind_rows(fbref_shots_keep,fbref_shots_new)
  
  fbref <-
    bind_rows(fbref_squad_player,fbref_shots)
  
  saveRDS(fbref,file=save_path)
  
  return(fbref)
}

fbref_get_selector <- function(page,seasoncode=NA,stattype=NA,statselector=NA,matchcode=NA){
  
  selector <-
    case_when(
      page=="player" ~ glue("%23stats_{statselector}"),
      page=="squad" ~ glue("%23stats_{statselector}_squads"),
      page=="schedule" ~ glue("%23sched_ks_{seasoncode}_1"),
      page=="league" ~ glue("%23results{seasoncode}1_overall"),
      page=="leagueha" ~ glue("%23results{seasoncode}1_home_away"),
      page=="match" & stattype=="shots" ~ glue("%23all_shots_all"),
      TRUE ~ glue()
    )
  return(selector)
}

fbref_get_url <- function(page,seasoncode=NA,stattype=NA,statselector=NA,matchcode=NA){
  
  url <-
    case_when(
      page %in% c("player","squad") ~ glue("https://fbref.com/en/comps/9/{seasoncode}/{stattype}/"),
      page=="schedule" ~ glue("https://fbref.com/en/comps/9/{seasoncode}/schedule/"),
      page %in% c("league","leagueha") ~ glue("https://fbref.com/en/comps/9/{seasoncode}/"),
      page=="match" ~ glue("https://fbref.com/en/matches/{matchcode}/"),
      TRUE ~ glue()
    )
  return(url)
}

fbref_scrape <- function(page_url=NA,content_selector_id=NA,page=NA,stattype=NA){
  url <- glue("http://acciotables.herokuapp.com/?page_url={page_url}&content_selector_id={content_selector_id}")
  print(glue("url: {url}"))
  
  # browser()
  
  session <- polite::bow(url,user_agent="@saintsbynumbers")
  data_html <- polite::scrape(session)
  
  data_table <-
    data_html %>%
    html_table(header=FALSE) %>%
    extract2(1)
  
  # clean names and remove non-data rows
  data_table <- fbref_clean_names(data_table,page)
  
  # add url codes
  data <- fbref_scrape_href(data_html,data_table,page)
  
  # need to also get page code here?
  
  return(data)
}

fbref_scrape_href <- function(data_html,data_table,page=NA){
  
  if(page %in% c("league")){
    
    data_href <-
      data_html %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      as_tibble() %>%
      separate(value,c(NA,NA,"datatype","code","desc"),sep="/",extra="merge",fill="right") %>%
      filter(datatype=="squads") %>%
      print
    
    data <-
      data_table %>%
      bind_cols(data_href) %>%
      left_join(data_table,.)
    
  } else if(page %in% c("schedule")){
    
    data_href <-
      data_html %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      as_tibble() %>%
      separate(value,c(NA,NA,"datatype","code","desc"),sep="/",extra="merge",fill="right") %>%
      filter(datatype=="matches") %>%
      filter(!is.na(desc)) %>%
      unique()
    
    data <-
      data_table %>%
      filter(match_report=="Match Report") %>%
      bind_cols(data_href) %>%
      left_join(data_table,.)
    
  } else {
    data <- data_table
  }
  
  return(data)
}

fbref_scrape_events <- function(code){
  selector <- ".event"
  url <- glue("https://fbref.com/en/matches/{code}/")
  print(url)
  
  # browser()
  
  events <-
    url %>%
    read_html() %>%
    html_nodes(selector)
  
  eventstext <- events %>%
    html_text() %>%
    str_remove_all("\n") %>%
    str_remove_all("\t") %>%
    str_squish() %>%
    as_tibble()
  
  eventsteams <- events %>%
    html_attr("class") %>%
    str_squish() %>%
    as_tibble() %>%
    rename("team"="value") %>%
    mutate(team=case_when(
      team=="event" ~ "event",
      team=="event a" ~ "Home",
      team=="event b" ~ "Away",
      TRUE ~ team
    ))
  
  # browser()
  
  eventstable <-
    cbind(eventstext,eventsteams) %>%
    slice(-1,-2) %>%
    separate(value,c("time","desc"),sep="&rsquor;",extra="merge",fill="right") %>%
    mutate(half=case_when(
      as.numeric(str_sub(time,1,2)) <= 45 ~ 1,
      TRUE ~ 2 #extra time?
    )) %>%
    mutate(time=case_when(
      str_detect(time,"\\+") ~ as.character(as.numeric(str_sub(time,1,2))+as.numeric(str_sub(time,3))),
      TRUE ~ time
    )) %>%
    separate(desc,c("home",NA,"away","desc"),sep=c(1,2,3),extra="merge",fill="right") %>%
    separate(desc,c("desc","type"),sep=" — ",extra="merge",fill="right") %>%
    mutate(type=case_when(
      str_detect(type,"Goal") ~ "Goal",
      str_detect(desc,"Penalty Kick") ~ "Goal (pen)",
      str_detect(desc,"Penalty Miss") ~ "Miss (pen)",
      TRUE ~ type
    )) %>%
    # mutate(state=as.numeric(home)-as.numeric(away)) %>%
    mutate(desc=str_remove_all(desc,coll("Penalty Kick — Substitute| —|Penalty Miss"))) %>%
    separate(desc,c("player1","player2"),sep=coll("for |Assist:|Penalty Kick —|Penalty Kick|Penalty saved by |Penalty Miss")) %>%
    # relocate(half,time,type,team,player1,player2,home,away) %>% #state on the end if used
    glimpse
  
  # browser()
  
  return(eventstable)
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
    
    data <-
      data %>%
      slice(-1,-2)
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
    
    data <-
      data %>%
      slice(-1)
  }
  if(page=="match"){ # should be stattype=="shots"
    names(data) <-
      glue("{data[1,]} {data[2,]}") %>%
      str_squish() %>%
      str_to_lower() %>%
      str_replace_all(c(" "="_","%"="pc","#"="n")) %>%
      make.unique(sep="_")
    
    data <-
      data %>%
      slice(-1,-2)
  }
  if("player" %in% names(data)){ # remove duplicated column names from player table
    data %<>% filter(player != "Player")
    data <-
      data %>%
      filter(player != "Player")
  }
  if("wk" %in% names(data)){ # remove duplicated column names + blank rows from schedule
    data <-
      data %>%
      filter(wk != "Wk") %>%
      filter(wk != "")
  }
  if(page=="match"){ # should be stattype=="shots"
    data <-
      data %>%
      filter(minute != "")
  }
  
  data <-
    data %>%
    type_convert # refactor data types
}

# fbref_scrape_events <- function(code){
#   url <- glue("https://fbref.com/matches/{code}/")
#   # print(url)
#   
#   return(url)
# }
