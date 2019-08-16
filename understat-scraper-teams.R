# get_league_teams_stats("EPL",2018) %>%
#   View("EPL 2018")

# get_team_players_stats("Southampton",2018) %>%
#   # View("Southampton 2018")

team_meta <- function(team_name) {
  
  team_name <- str_replace_all(team_name, " ", "_")
  
  team_url <- str_glue("https://understat.com/team/{team_name}")
  
  # read understat team page
  team_page <- read_html(team_url)
  
  # isolate league links
  year_link <- html_nodes(team_page, "#header :nth-child(2)")
  
  # isolate year options
  year_options <- html_nodes(year_link[2], "option")
  
  # create league fields as df
  team_df <- data.frame(
    team_name = team_name,
    year = as.numeric(html_attr(year_options, "value")),
    season = html_text(year_options),
    stringsAsFactors = FALSE
  )
  
  # create url per season
  team_df$url <- file.path(team_url, team_df$year)
  
  return(team_df)
  
}

team_meta("Newcastle United") %>%
  View("NU")
