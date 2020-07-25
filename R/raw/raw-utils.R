fbref_get_selector <- function(page,seasoncode=NA,stattype=NA,statselector=NA){
  
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
  
  Sys.sleep(2)
  
  # import table
  data_html <-
    url %>%
    read_html()
  
  # read table
  # data_table <-
  #   data_html %>%
  #   html_table(header=FALSE) %>%
  #   extract2(1)
  
  # browser()
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
  # browser()
  
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
    
    # d <- nrow(data_table) - nrow(data_href)
    
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

# fbref_scrape <- function(page_url,content_selector_id, datatype){
#   url <- glue("http://acciotables.herokuapp.com/?page_url={page_url}&content_selector_id={content_selector_id}")
#   print(glue("url: {url}"))
#   
#   # browser()
#   
#   data <- case_when(
#     datatype=="urlcode" ~ glue("datatype = urlcode!"),
#     TRUE ~
#       url %>%
#       read_html() %>%
#       html_table(header=FALSE) %>%
#       extract2(1)
#   )
#   
#   print(data)
#   
#   # data <-
#   #   url %>%
#   #   read_html() %>%
#   #   html_table(header=FALSE) %>%
#   #   extract2(1)
#   
#   return(data)
# }

# fbref_scrape_href <- function(page_url,content_selector_id){
#   url <- glue("http://acciotables.herokuapp.com/?page_url={page_url}&content_selector_id={content_selector_id}")
#   print(glue("url: {url}"))
#   
#   data <-
#     url %>%
#     read_html() %>%
#     html_nodes("a") %>%
#     html_attr("href") %>%
#     as_tibble() %>%
#     print
#   
#   return(data)
# }

# fbref_get_codes_squads <- function(eplseasons){
#   
#   # browser()
#   
#   data <- tibble(page="league") %>%
#     crossing(.eplseasons) %>%
#     mutate(page_url=fbref_get_url(page,seasoncode)) %>%
#     mutate(content_selector_id=fbref_get_selector(page,seasoncode)) %>%
#     mutate(data=map2(page_url,content_selector_id,fbref_scrape_href)) %>%
#     unnest(cols=data) %>%
#     separate(value,c(NA,NA,"datatype","code","desc"),sep="/",extra="merge",fill="right") %>%
#     separate(desc,c(NA,"desc"),sep="/",fill="left") %>%
#     filter(datatype=="squads") %>%
#     select(season,datatype,code,desc)
#   
#   return(data)
# }

# fbref_get_codes_matches <- function(eplseasons){
#   
#   data <- tibble(page="schedule") %>%
#     crossing(.eplseasons) %>%
#     mutate(page_url=fbref_get_url(page,seasoncode)) %>%
#     mutate(content_selector_id=fbref_get_selector(page,seasoncode)) %>%
#     mutate(data=map2(page_url,content_selector_id,fbref_scrape_href)) %>%
#     # unnest(cols=data) %>%
#     # separate(value,c(NA,NA,"datatype","code","data"),sep="/",extra="merge",fill="right") %>%
#     # separate(data,c(NA,"data"),sep="/",fill="left") %>%
#     # filter(datatype=="squads") %>%
#     unnest(cols=data) %>%
#     separate(value,c(NA,NA,"datatype","code","desc"),sep="/",extra="merge",fill="right") %>%
#     filter(!is.na(desc)) %>%
#     filter(datatype=="matches") %>%
#     unique() %>%
#     # print
#     select(season,datatype,code,desc) %>%
#     print
#   
#   return(data)
# }

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
  
  # browser()
  
  if(page %in% c("squad","player","schedule","league","leagueha")){ # everything
    data %<>%
      select(-any_of(c("rk","matches","notes","match_report","top_team_scorer","goalkeeper"))) %>%
      select(-contains(c("pc","90"))) %>%
      select(-any_of(c("datatype","desc")))
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

