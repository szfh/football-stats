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

fbref_scrape_href <- function(page_url,content_selector_id){
  url <- glue("http://acciotables.herokuapp.com/?page_url={page_url}&content_selector_id={content_selector_id}")
  print(glue("url: {url}"))
  
  data <-
    url %>%
    read_html() %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as_tibble() %>%
    print
  
  return(data)
}

fbref_get_codes_squads <- function(eplseasons){
  
  # browser()
  
  data <- tibble(page="league") %>%
    crossing(.eplseasons) %>%
    mutate(page_url=fbref_get_url(page,seasoncode)) %>%
    mutate(content_selector_id=fbref_get_selector(page,seasoncode)) %>%
    mutate(data=map2(page_url,content_selector_id,fbref_scrape_href)) %>%
    unnest(cols=data) %>%
    separate(value,c(NA,NA,"datatype","code","desc"),sep="/",extra="merge",fill="right") %>%
    separate(desc,c(NA,"desc"),sep="/",fill="left") %>%
    filter(datatype=="squads") %>%
    select(season,datatype,code,desc)
  
  return(data)
}

fbref_get_codes_matches <- function(eplseasons){
  
  data <- tibble(page="schedule") %>%
    crossing(.eplseasons) %>%
    mutate(page_url=fbref_get_url(page,seasoncode)) %>%
    mutate(content_selector_id=fbref_get_selector(page,seasoncode)) %>%
    mutate(data=map2(page_url,content_selector_id,fbref_scrape_href)) %>%
    # unnest(cols=data) %>%
    # separate(value,c(NA,NA,"datatype","code","data"),sep="/",extra="merge",fill="right") %>%
    # separate(data,c(NA,"data"),sep="/",fill="left") %>%
    # filter(datatype=="squads") %>%
    unnest(cols=data) %>%
    separate(value,c(NA,NA,"datatype","code","desc"),sep="/",extra="merge",fill="right") %>%
    filter(!is.na(desc)) %>%
    filter(datatype=="matches") %>%
    unique() %>%
    # print
    select(season,datatype,code,desc) %>%
    print
  
  return(data)
}
