library(tidyverse)

fivethirtyeight <- read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv")

fte_leagues <- fivethirtyeight %>%
  select(league,league_id) %>%
  unique()

fivethirtyeight %>%
  filter(league_id=="2411") %>%
  filter(is.na(xg1)==FALSE) %>%
  tail(50) %>%
  view