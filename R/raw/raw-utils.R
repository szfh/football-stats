get_data_types <- function(){
  data_types <- list()
  data_types$season <- tibble(season=2018:2023)
  data_types$country <- tibble(country="ENG")
  # data_types$gender <- tibble(gender="M")
  data_types$advanced_stats <- tibble(stat=c("summary","passing","passing_types","defense" ,"possession","misc","keeper"))
  data_types$season_team_stats <- tibble(stat=c("league_table", "league_table_home_away", "standard", "keeper",
                                                "keeper_adv", "shooting", "passing", "passing_types", "goal_shot_creation",
                                                "defense", "possession", "playing_time", "misc"))
  data_types$team_or_player <- tibble(team_or_player=c("player","team"))
  
  return(data_types)
}

pmap_pbar <- function(.l, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <-
    progress::progress_bar$new(
      format = "[:bar] :current/:total taken :elapsed eta :eta rate :tick_rate/sec",
      total = length(.l[[1]]),
      force = TRUE
    )
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::pmap(.l, f, ...)
}