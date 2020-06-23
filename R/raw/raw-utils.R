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

understat_scrape_league <- function(league="EPL", year="2019", str){
  url <- glue("https://understat.com/league/{league}/{year}")
  print(glue("url: {url}"))
  
  data <-
    url %>%
    read_html() %>%
    html_nodes("script") %>%
    as.character() %>%
    stringr::str_subset(str) %>%
    stringi::stri_unescape_unicode() %>%
    stringr::str_extract("\\[.+\\]") %>%
    jsonlite::fromJSON(simplifyVector=TRUE)
  
  return(data)
}
