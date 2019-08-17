# https://github.com/ewenme/understatr/blob/master/R/leagues.R

#function to get available seasons for each league
league_seasons <- function(league_name){
  # get league url
  league_url <- paste0("https://understat.com/league/",league_name)

  # get year link
  year_link <- read_html(league_url) %>%
    html_nodes("#header :nth-child(2)")
  
  # isolate year options
  year_options <- html_nodes(year_link[2], "option")
  
  # create league fields as df
  seasons_df <- data.frame(
    league_name = league_name,
    year = as.numeric(html_attr(year_options, "value")),
    season = html_text(year_options),
    stringsAsFactors = FALSE
  )
  
  league_seasons <- data.frame(
    league_name = league_name,
    year = as.numeric(html_attr(year_options, "value")),
    season = html_text(year_options),
    stringsAsFactors = FALSE
  )
  
  # as tibble and create league url
  league_seasons %<>%
    as_tibble %>%
    mutate(url=file.path(league_url, year))
  return(league_seasons)
}

#function to get available seasons for a league
seasons <- function(){
  #read home page into html
  home_page <- read_html(url_home_page)
  
  #extract league urls
  league_url <- html_nodes(home_page,".link") %>%
    html_attr("href")
  #extract league names
  league_names <- html_nodes(home_page,".link") %>%
    html_text()
  
  #turn into tibble
  seasons <- map_dfr(
    league_names, league_seasons)
  
  seasons %<>%
    as_tibble
  
  return(seasons)
}

