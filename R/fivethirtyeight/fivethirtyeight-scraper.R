library(tidyverse)

spi_matches <- read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv")
spi_rankings <- read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_global_rankings.csv")

spi_leagues <- spi_matches %>%
  select(league,league_id) %>%
  unique()

spi_matches %>%
  filter(league_id=="2411") %>%
  filter(is.na(xg1)==FALSE) %>%
  top_n(50,date) %>%
  view

spi_matches %>%
  filter(team1=="Southampton"|team2=="Southampton") %>%
  view()
