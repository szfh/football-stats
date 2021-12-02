{
  source(here::here("R","library.R"),encoding="utf-8")
  source(here("R","join","join.R"),encoding="utf-8")
  source(here("R","themes.R"),encoding="utf-8")
  source(here("R","plot","plot-utils.R"),encoding="utf-8")
}
{
  data <- join()
}
{
  plots <- list()
  team <- "Southampton"
  season <- "2020-2021"
  player <- ""
}
{
  lapply(list.files(here("R","plot","team-plots"), full.names=TRUE), source, encoding="utf-8")
  plots$minutes <- team_minutes(team,season)
  plots$xg_trend <- xg_trend(team,season)
  plots$xg_segment <- xg_segment(team,season)
  plots$xg_xa <- xg_xa(team,season)
  plots$shots_key_passes <- shots_key_passes(team,season)
  plots$psxg_against <- psxg_against(team,season)
}

{
  plots_logo <-
    plots %>%
    add_logo(path=here("images","SB_Regular.png"),x=1,y=1,hjust=1.1,width=0.2) %>%
    add_logo(path=here("images","fbref.png"),x=0.88,y=1,hjust=1.1,width=0.29)
  
  save_plots(plots_logo,path=here("plots","team"))
}
