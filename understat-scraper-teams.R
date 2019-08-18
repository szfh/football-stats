# https://github.com/ewenme/understatr/blob/master/R/teams.R
# Get information on a team's available seasons
team_meta <- function(team_name) {
  
  # team_name <- str_replace_all(team_name, " ", "_") #old
  
  # team_url <- str_glue("https://understat.com/team/{team_name}") #old
  team_url <- paste0("https://understat.com/team/",team_name) %>%
    str_replace_all(" ", "_")
  
  # read understat team page
  # team_page <- read_html(team_url)
  
  # isolate league links
  year_link <- read_html(team_url) %>%
    html_nodes("#header :nth-child(2)")
  
  # isolate year options
  year_options <- html_nodes(year_link[2], "option")
  
  # create league fields as df
  team_meta <- data.frame(
    team_name = team_name,
    year = as.numeric(html_attr(year_options, "value")),
    season = html_text(year_options),
    stringsAsFactors = FALSE
  )
  
  team_meta %<>%
    mutate(url=file.path(team_url, year)) %>%
    as_tibble
  # create url per season
  # team_df$url <- file.path(team_url, team_df$year)
  
  return(team_meta)
}

# Get stats for a team's players for a given season
team_players_stats <- function(team_name, year) {
  
  # stopifnot(is.character(team_name))
  
  # team_name <- str_replace_all(team_name, " ", "_")
  
  # construct team url
  # team_url <- str_glue("https://understat.com/team/{team_name}/{year}")
  team_url <- paste0("https://understat.com/team/",team_name,"/",year) %>%
    str_replace_all(" ", "_")
  
  # read team page
  team_page <- read_html(team_url)
  
  players_data <- team_page %>%
    # locate script tags
    html_nodes("script") %>%
    as.character() %>%
    # isolate player data
    str_subset("playersData") %>%
    # fix encoding
    stri_unescape_unicode() %>%
    # pick out JSON string
    rm_square(extract = TRUE, include.markers = TRUE) %>%
    unlist() %>%
    str_subset("\\[\\]", negate = TRUE) %>%
    # parse JSON
    fromJSON()
  
  # add reference fields
  players_data$year <- as.numeric(year)
  names(players_data)[names(players_data) == 'team_title'] <- 'team_name'
  names(players_data)[names(players_data) == 'id'] <- 'player_id'
  
  # fix col classes
  players_data <- type.convert(players_data)
  players_data[] <- lapply(players_data, function(x) if(is.factor(x)) as.character(x) else x)
  
  return(as_tibble(players_data))
}