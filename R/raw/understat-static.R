raw <- list()

league <- c("EPL")
year <- c(2017,2018)

raw[["understat"]][["teams"]] <- 
  expand_grid(league,year) %>% 
  transmute(
    data = map2(league, year, understatr::get_league_teams_stats)
  ) %>%
  unnest(data)

raw[["understat"]][["teamssum"]] <- raw[["understat"]][["teams"]] %>%
  group_by(team_name, year) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

raw[["understat"]][["players"]] <-
  raw[["understat"]][["teams"]] %>%
  distinct(league_name, year, team_name) %>% 
  # top_n(2) %>%
  transmute(
    data = map2(team_name, year, understatr::get_team_players_stats)
  ) %>%
  unnest(data)

saveRDS(raw,file=here("data","understat-static.rds"))
rm(raw)
