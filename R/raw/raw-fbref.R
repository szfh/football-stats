scrape_fbref <- function(save_path=here("data","fbref.rds"),current_season="2020-21"){
  fbref_saved <- readRDS(save_path)
  
  data_types <- list()
  
  data_types$seasons <- tribble(~season, ~season_key, #advanced/non-advanced?
                                "2020-21",10728,
                                "2019-20",3232,
                                "2018-19",1889,
                                "2017-18",1631,
                                # "2016-17",1526,
  )
  
  data_types$squad_player <- tribble(~page,
                                     "player",
                                     "squad",
  )
  
  data_types$squad_player_tables <- tribble(~stat, ~stat_key, # add %23 to statselector? make factor/ordered?
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
  
  data_types$league <- tribble(~page,
                               "schedule",
                               "league",
                               "leagueha",
  )
  
  fbref_data <- list()
  fbref <- list()
  
  fbref_data$squad_player$all <-
    tibble() %>% # all squad + player data parameters
    bind_rows(crossing(data_types$squad_player,data_types$squad_player_tables)) %>%
    bind_rows(data_types$league) %>%
    crossing(data_types$seasons)
  
  fbref_data$squad_player$keep <-
    fbref_saved %>%
    filter(page %in% c("player","squad","league","leagueha","schedule")) %>%
    filter(season!=current_season)
  
  fbref_data$squad_player$new <-
    anti_join(fbref_data$squad_player$all, fbref_data$squad_player$keep) %>%
    mutate(page_url=fbref_get_url(page,season_key,stat,stat_key)) %>% # don't need stat?
    mutate(content_selector_id=fbref_get_selector(page,season_key,stat,stat_key)) %>% #don't need stat?
    mutate(data=pmap(list(page_url, content_selector_id, page, stat), possibly(fbref_scrape, otherwise=NA)))
  
  fbref$squad_player <-
    bind_rows(fbref_data$squad_player$keep,fbref_data$squad_player$new) %>%
    filter(!is.na(data))
  
  fbref_data$matches$all <-
    fbref_saved %>%
    filter(page=="schedule") %>%
    select(page,stat,stat_key,season,season_key,page_url,content_selector_id,data) %>%
    unnest(cols=data) %>%
    select(page:season_key,home,away,match_key=code) %>% # fix match_key=code?
    mutate(page="match",stat="events") %>% # crossing?
    filter(!is.na(match_key))
  
  fbref_data$events$keep <-
    fbref_saved %>%
    filter(stat=="events") %>%
    filter(!is.na(data)) %>%
    filter(!is.null(data))
  
  fbref_data$events$new <-
    anti_join(fbref_data$matches$all, fbref_data$events$keep, by="match_key") %>%
    mutate(stat="events") %>% #delete?
    mutate(page_url=fbref_get_url(page,match_key=match_key)) %>%
    mutate(data=pmap(list(page_url,page,stat), possibly(fbref_scrape_events, otherwise=NA)))
  
  fbref$events <-
    bind_rows(fbref_data$events$keep,fbref_data$events$new)
  
  fbref_data$shots$keep <-
    fbref_saved %>%
    filter(stat=="shots") %>%
    filter(!is.na(data)) %>%
    filter(!is.null(data))
  
  fbref_data$shots$new <-
    anti_join(fbref_data$matches$all, fbref_data$shots$keep, by="match_key") %>%
    mutate(stat="shots") %>%
    mutate(page_url=fbref_get_url(page,match_key=match_key)) %>%
    mutate(content_selector_id=fbref_get_selector(page,stat=stat,match_key=match_key)) %>%
    mutate(data=pmap(list(page_url,content_selector_id,page,stat), possibly(fbref_scrape, otherwise=NA)))
  
  fbref$shots <-
    bind_rows(fbref_data$shots$keep,fbref_data$shots$new)
  
  fbref_all <-
    bind_rows(fbref$squad_player,fbref$events,fbref$shots)
  
  saveRDS(fbref_all,file=save_path)
  
  return(fbref_all)
}

# don't need stat and stat_key?
fbref_get_selector <- function(page,season_key=NA,stat=NA,stat_key=NA,match_key=NA){
  
  selector <-
    case_when(
      page=="player" ~ glue("%23stats_{stat_key}"),
      # page=="squad" ~ glue("%23stats_{stat_key}_squads"),
      page=="squad" ~ glue("%23stats_squads_{stat_key}_for"),
      page=="schedule" ~ glue("%23sched_{season_key}_1"),
      page=="league" ~ glue("%23results{season_key}1_overall"),
      page=="leagueha" ~ glue("%23results{season_key}1_home_away"),
      page=="match" & stat=="shots" ~ glue("%23all_shots_all"),
      TRUE ~ glue()
    )
  return(selector)
}

fbref_get_url <- function(page,season_key=NA,stat=NA,stat_key=NA,match_key=NA){
  url <-
    case_when(
      page %in% c("player","squad") ~ glue("https://fbref.com/en/comps/9/{season_key}/{stat}/"),
      page=="schedule" ~ glue("https://fbref.com/en/comps/9/{season_key}/schedule/"),
      page %in% c("league","leagueha") ~ glue("https://fbref.com/en/comps/9/{season_key}/"),
      page=="match" ~ glue("https://fbref.com/en/matches/{match_key}/"),
      TRUE ~ glue()
    )
  return(url)
}

fbref_scrape <- function(page_url=NA,content_selector_id=NA,page=NA,stat=NA){
  url <- glue("http://acciotables.herokuapp.com/?page_url={page_url}&content_selector_id={content_selector_id}")
  # print(glue("url: {url}"))
  
  session <- polite::bow(url,user_agent="@saintsbynumbers")
  data_html <- polite::scrape(session)
  
  data_table <-
    data_html %>%
    html_table(header=FALSE) %>%
    extract2(1)
  
  # clean names and remove non-data rows
  data_table <- fbref_clean_names(data_table,page,stat)
  
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
      filter(datatype=="squads")
    
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

fbref_scrape_events <- function(url,page,stat){
  selector <- ".event"
  # print(glue("url: {url}"))
  
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

fbref_clean_names <- function(data,page,stat=NA){
  if(page %in% c("squad","player","leagueha")){
    names(data) <-
      glue("{data[1,]} {data[2,]}") %>%
      str_squish() %>%
      str_to_lower() %>%
      str_replace_all(c(" "="_","%"="pc","#"="n")) %>%
      str_remove_all("[/ \\( \\)]") %>%
      make.unique(sep="_")
    
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
      make.unique(sep="_")
    
    data <-
      data %>%
      slice(-1)
  }
  if(page %in% "match" & stat %in% "shots"){ # should be stat=="shots"
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
  if(page %in% "match" & stat %in% "events"){
    data <-
      data
  }
  if(page %in% "match" & stat %in% "shots"){ # should be stat=="shots"
    data <-
      data %>%
      filter(minute != "")
  }
  
  data <-
    data %>%
    type_convert # refactor data types
}
