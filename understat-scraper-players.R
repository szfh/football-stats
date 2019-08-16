# get_player_matches_stats(843) %>%
#   View("JWP matches")

# get_player_seasons_stats(843) %>%
# View("JWP seasons")

get_player_seasons_stats <- function(player_id) {
  
  # construct player url
  player_url <- str_glue("https://understat.com/player/{player_id}")
  
  # read player page
  player_page <- read_html(player_url)
  
  player_data <- player_page %>%
    # locate script tags
    html_nodes("script") %>%
    as.character() %>%
    # isolate player data
    str_subset("groupsData") %>%
    # fix encoding
    stri_unescape_unicode() %>%
    # pick out JSON string
    rm_square(extract = TRUE, include.markers = TRUE) %>%
    unlist() %>%
    str_subset("\\[\\]", negate = TRUE) %>%
    # parse JSON
    fromJSON()
  
  # get player name
  player_name <- player_page %>%
    html_nodes(".header-wrapper:first-child") %>%
    html_text() %>%
    trimws()
  
  # add reference fields
  player_data$player_id <- as.numeric(player_id)
  player_data$player_name <- player_name
  names(player_data)[names(player_data) == 'team'] <- 'team_name'
  names(player_data)[names(player_data) == 'season'] <- 'year'
  
  # fix col classes
  player_data <- type.convert(player_data)
  player_data[] <- lapply(player_data, function(x) if(is.factor(x)) as.character(x) else x)
  
  return(as_tibble(player_data))
  
}