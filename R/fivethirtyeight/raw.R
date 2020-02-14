source(here::here("R","library.R"))

if (!exists("raw",inherits=FALSE)){
  raw <- list()
}

spi_matches <- read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv")
spi_rankings <- read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_global_rankings.csv")



raw[["fivethirtyeight"]][["matches"]] <- read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv")
raw[["fivethirtyeight"]][["rankings"]] <- read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_global_rankings.csv")

# raw[["fivethirtyeight"]][["matches"]] %>%
#   select(league,league_id) %>%
#   unique() %>% 
#   view()

# raw[["fivethirtyeight"]][["matches"]] %>%
#   filter(league_id=="2411") %>%
#   filter(is.na(xg1)==FALSE) %>%
#   top_n(50,date) %>%
#   view()

# raw[["fivethirtyeight"]][["matches"]] %>%
#   filter(team1=="Southampton"|team2=="Southampton") %>%
#   view()
