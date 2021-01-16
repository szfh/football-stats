scrape_fbref <- function(save_path=here("data","fbref.rds"),current_season="2020-21"){
  fbref_saved <- readRDS(save_path)
  
  browser()
  
  eplseasons <- tribble(~season, ~key_season, #advanced/non-advanced?
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
    filter(!is.na(matchcode))
  
  fbref_events_keep <-
    fbref_saved %>%
    filter(stattype=="events") %>%
    filter(!is.na(data)) %>%
    filter(!is.null(data))
  
  fbref_events_new <-
    anti_join(fbref_matches_all, fbref_events_keep, by="matchcode") %>%
    mutate(stattype="events") %>%
    mutate(page_url=fbref_get_url(page,matchcode=matchcode)) %>%
    mutate(data=pmap(list(page_url,page,stattype), possibly(fbref_scrape_events, otherwise=NA)))
  
  fbref_events <-
    bind_rows(fbref_events_keep,fbref_events_new)
  
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
    bind_rows(fbref_squad_player,fbref_events,fbref_shots)
  
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
  
  session <- polite::bow(url,user_agent="@saintsbynumbers")
  data_html <- polite::scrape(session)
  
  data_table <-
    data_html %>%
    html_table(header=FALSE) %>%
    extract2(1)
  
  # clean names and remove non-data rows
  data_table <- fbref_clean_names(data_table,page,stattype)
  
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
    data <-
      data_table
  }
  
  return(data)
}

fbref_scrape_events <- function(url,page,stattype){
  selector <- ".event"
  print(url)
  
  events_raw <-
    url %>%
    read_html() %>%
    html_nodes(selector)
  
  events_text <-
    events_raw %>%
    html_text() %>%
    as_tibble() %>%
    rename("event"="value")
  
  events_teams <-
    events_raw %>%
    html_attr("class") %>%
    as_tibble() %>%
    rename("type"="value")
  
  events <-
    bind_cols(events_text,events_teams)
  
  return(events)
}

fbref_clean_names <- function(data,page,stattype=NA){
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
  if(page %in% "match" & stattype %in% "shots"){ # should be stattype=="shots"
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
  if("player" %in% names(data)){ # remove duplicated column names from player table # player1 for events?
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
  if(page %in% "match" & stattype %in% "events"){
    data <-
      data
  }
  if(page %in% "match" & stattype %in% "shots"){ # should be stattype=="shots"
    data <-
      data %>%
      filter(minute != "")
  }
  
  data <-
    data %>%
    type_convert # refactor data types
}
