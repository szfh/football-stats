get_data_types <- function(){
  data_types <- list()
  data_types$season <- tibble(season=2018:2022)
  data_types$country <- tibble(country="ENG")
  # data_types$gender <- tibble(gender="M")
  data_types$advanced_stats <- tibble(stat=c("summary","passing","passing_types","defense" ,"possession","misc","keeper"))
  data_types$season_team_stats <- tibble(stat=c("league_table", "league_table_home_away", "standard", "keeper",
                                                "keeper_adv", "shooting", "passing", "passing_types", "goal_shot_creation",
                                                "defense", "possession", "playing_time", "misc"))
  data_types$team_or_player <- tibble(team_or_player=c("player","team"))
  
  return(data_types)
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
  print(glue("url: {url}"))
  # browser()
  session <- polite::bow(url,user_agent="@saintsbynumbers")
  data_html <- polite::scrape(session)
  # browser()
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
