source(here::here("R","fbref","library.R"))
source(here("R","fbref","tidy.R"))
source(here("R","fivethirtyeight","scraper.R"))

change_name <- function(team){
  # browser()
  team <- case_when(
    team=="AFC Bournemouth" ~ "Bournemouth",
    team=="Brighton and Hove Albion" ~ "Brighton",
    team=="Manchester United" ~ "Manchester Utd",
    team=="Newcastle" ~ "Newcastle Utd",
    team=="Sheffield United" ~ "Sheffield Utd",
    team=="Tottenham Hotspur" ~ "Tottenham",
    team=="West Ham United" ~ "West Ham",
    team=="Wolverhampton" ~ "Wolves",
    TRUE ~ team
  )
  
  return(team)

}

spi_matches_rn <- spi_matches %>%
  filter(league_id==2411) %>%
  mutate(
    "date"=as.Date(date),
    "team1"=as.character(team1),
    "team2"=as.character(team2),
    "team1"=sapply(team1,change_name),
    "team2"=sapply(team2,change_name),
  ) %>%
  rename(
    "Home"="team1",
    "Away"="team2",
    "Date"="date",
    )

# spi_matches_rn %>%
#   select(Home) %>%
#   unique() %>%
#   view()

matches_join <- matches %>%
  left_join(spi_matches_rn) %>% 
  filter(!is.na(score1)) %>%
  # select(Home) %>%
  # unique() %>%
  view
