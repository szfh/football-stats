raw <- readRDS(file=here("data","raw-fivethirtyeight.rds"))

change_name <- function(team){

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

if (!exists("tidy",inherits=FALSE)){
  tidy <- list()
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

rm(change_name)
