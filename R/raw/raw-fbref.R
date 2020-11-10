scrape_fbref <- function(save_path=here("data","fbref.rds"),current_season="2020-21"){
  
  fbref_saved <- readRDS(save_path)
  
  eplseasons <- tribble(~season, ~seasoncode, #advanced/non-advanced?
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
  
  fbref_all <- tibble() %>% # all squad + player data parameters
    bind_rows(crossing(data_types_squad_player,tables_squad_player)) %>% # (squad + player) * datatypes
    bind_rows(data_types_league) %>% # fixtures
    crossing(eplseasons)
  
  fbref_keep <- fbref_saved %>% # remove data to be scraped from saved
    filter(season!=current_season)
  
  fbref_new <-
    anti_join(fbref_all, fbref_keep) %>%
    mutate(page_url=fbref_get_url(page,seasoncode,stattype,statselector)) %>%
    mutate(content_selector_id=fbref_get_selector(page,seasoncode,stattype,statselector)) %>%
    print
  
  fbref_new %<>%
    mutate(data=pmap(list(page_url, content_selector_id, page, stattype), possibly(fbref_scrape, otherwise=NA)))
  
  fbref <- bind_rows(fbref_keep,fbref_new) %>%
    filter(!is.na(data))
  
  saveRDS(fbref,file=save_path)
  
  return(fbref)
}

fbref_get_selector <- function(page,seasoncode=NA,stattype=NA,statselector=NA){
  
  selector <- case_when(
    page=="player" ~ glue("%23stats_{statselector}"),
    page=="squad" ~ glue("%23stats_{statselector}_squads"),
    page=="schedule" ~ glue("%23sched_ks_{seasoncode}_1"),
    page=="league" ~ glue("%23results{seasoncode}1_overall"),
    page=="leagueha" ~ glue("%23results{seasoncode}1_home_away"),
    TRUE ~ glue()
  )
  return(selector)
}

fbref_get_url <- function(page,seasoncode=NA,stattype=NA,statselector=NA){
  
  url <- case_when(
    page %in% c("player","squad") ~ glue("https://fbref.com/en/comps/9/{seasoncode}/{stattype}/"),
    page=="schedule" ~ glue("https://fbref.com/en/comps/9/{seasoncode}/schedule/"),
    page %in% c("league","leagueha") ~ glue("https://fbref.com/en/comps/9/{seasoncode}/"),
    TRUE ~ glue()
  )
  return(url)
}

fbref_scrape <- function(page_url=NA,content_selector_id=NA,page=NA,stattype=NA){
  url <- glue("http://acciotables.herokuapp.com/?page_url={page_url}&content_selector_id={content_selector_id}")
  print(glue("url: {url}"))
  
  # import table
  data_html <-
    url %>%
    read_html()
  
  session <- polite::bow(url,user_agent="@saintsbynumbers")
  data_html <- polite::scrape(session)
  
  data_table <- data_html %>%
    html_table(header=FALSE) %>%
    extract2(1)
  
  # clean names and remove non-data rows
  data_table <- fbref_clean_names(data_table,page)
  
  # add url codes
  data <- fbref_scrape_href(data_html,data_table,page)
  
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
    
    data <- cbind(data_table,data_href)
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

    if(nrow(data_table) - nrow(data_href) > 0){
      data_href <- data_href %>%
        add_row(code=(rep(NA,nrow(data_table) - nrow(data_href)))) #extend df
    }
    data <- cbind(data_table,data_href)
  } else {
    data <- data_table
  }
  return(data)
}

fbref_scrape_events <- function(matchcode){
  NULL
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
}
