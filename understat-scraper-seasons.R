library(understatr)

# get_leagues_meta() %>%
#   View("Leagues_old")

# https://github.com/ewenme/understatr/blob/master/R/leagues.R
#function to get available seasons for each league
league_seasons <- function(league_name){
  # construct league url
  league_url <- str_glue("https://understat.com/league/{league_name}")
  
  # read league page
  league_page <- read_html(league_url)
  
  # pick year link
  year_link <- html_nodes(league_page, "#header :nth-child(2)")
  
  # isolate year options
  year_options <- html_nodes(year_link[2], "option")
  
  # create league fields as df
  seasons_df <- data.frame(
    league_name = league_name,
    year = as.numeric(html_attr(year_options, "value")),
    season = html_text(year_options),
    stringsAsFactors = FALSE
  )
  
  # create url per season
  seasons_df$url <- file.path(league_url, seasons_df$year)
  
  return(seasons_df)
}

#function to get available seasons for a league
leagues <- function(){
  home_page <- read_html(home_page_url)
  league_url <- html_nodes(home_page,".link") %>%
    html_attr("href")
  league_names <- html_nodes(home_page,".link") %>%
    html_text()
  
  league_df <- map_dfr(
    league_names, get_league_seasons)
  
  return(as_tibble(league_df))
}

leagues %>%
  View()

