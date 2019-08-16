# library(understatr)

# get_leagues_meta() %>%
#   View("Leagues")

# get_league_teams_stats("EPL",2018) %>%
#   View("EPL 2018")
# 
# get_team_players_stats("Southampton",2018) %>%
#   # View("Southampton 2018")
# 
# get_player_matches_stats(843) %>%
#   View("JWP matches")
# 
# get_player_seasons_stats(843) %>%
#   View("JWP seasons")

# global objects
home_page_url <- "https://understat.com"

leagues <- function(){
  home_page <- read_html(home_page_url)
  league_url <- html_nodes(home_page,".link") %>%
    html_attr("href")
  league_names <- html_nodes(home_page,".link") %>%
    html_text()
}