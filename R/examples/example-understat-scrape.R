# remotes::install_github('ewenme/understatr')
require(tidyverse)
require(understatr)
require(rvest)
require(glue)
require(stringi)
require(stringr)
require(jsonlite)

understat_scrape_league <- function(league="EPL", year="2019", str="datesData"){
  url <- glue("https://understat.com/league/{league}/{year}")
  
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

## match stats/shots

# 1 row per match
matches <- understat_scrape_league(league="EPL", year=2019, str="datesData") # 1 row per match

# get match id
match_id <- matches %>% filter(isResult) %>% slice(1:10) %>% pull(id)

# get shots for each match - this is scraping 380 matches a season, so be patient
shots <- map_dfr(match_id, understatr::get_match_shots)

# same for match stats
stats <- map_dfr(match_id, understatr::get_match_stats)

## player stats

# 1 row per player
players <- understat_scrape_league(league="Bundesliga", year="2019", str="playersData")

teams <- understat_scrape_league(str="teamsData")

players %>%
  select(id) %>%
  mutate(shots=possibly())

# get player id
player_id <- players %>%
  # slice(1:4) %>%
  pull(id)

# get shots for player
all_shots <- player_id %>%
  map_dfr(.,possibly(get_player_shots,otherwise=NULL))
