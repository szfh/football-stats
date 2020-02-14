raw <- readRDS(file=here("data","raw-fivethirtyeight.rds"))

if (!exists("tidy",inherits=FALSE)){
  tidy <- list()
}

# source(here::here("R","fbref","library.R"))
# source(here("R","fbref","tidy.R"))
# source(here("R","fivethirtyeight","scraper.R"))

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

tidy[["fivethirtyeight"]][["matches"]] <- raw[["fivethirtyeight"]][["matches"]] %>%
  filter(league_id==2411) %>%
  mutate(
    "date"=as.Date(date),
  ) %>%
  mutate_at(c("team1","team2"),as.character) %>%
  mutate_at(c("team1","team2"),change_name) %>%
  rename(
    "Home"="team1",
    "Away"="team2",
    "Date"="date",
  ) %>%
  select (-c("score1","score2","league_id"))

# spi_matches_rn <- spi_matches %>%
#   filter(league_id==2411) %>%
#   mutate(
#     "date"=as.Date(date),
#   ) %>%
#   mutate_at(c("team1","team2"),as.character) %>%
#   mutate_at(c("team1","team2"),change_name) %>%
#   rename(
#     "Home"="team1",
#     "Away"="team2",
#     "Date"="date",
#     )

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
