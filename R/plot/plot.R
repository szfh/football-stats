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
  season <- expand_seasons("2021-2022")
  # season <- expand_seasons(c("2020-2021","2021-2022"))
  player <- ""
}
{
  lapply(list.files(here("R","plot","team-plots"), full.names=TRUE), source, encoding="utf-8")
  plots <- list()
  plots$minutes <- team_minutes(team,season)
  plots$xg_trend <- xg_trend(team,season=c("2020-2021","2021-2022"),lastn=38)
  plots$xg_segment <- xg_segment(team,season=c("2020-2021","2021-2022"),lastn=38)
  plots$xg_xa <- xg_xa(team,season,per90=FALSE)
  plots$xg_xa_p90 <- xg_xa(team,season,per90=TRUE)
  plots$shots_key_passes <- shots_key_passes(team,season)
  plots$psxg_against <- psxg_against(team,season)
  plots$pass_footedness <- pass_footedness(team,season)
  plots$subs <- subs(team,season=c("2020-2021","2021-2022"),lastn=38)
  
  plots_logo <-
    plots %>%
    add_logo(path=here("images","SB_Regular.png"),x=1,y=1,hjust=1.1,width=0.2) %>%
    add_logo(path=here("images","fbref.png"),x=0.88,y=1,hjust=1.1,width=0.29)
  
  save_plots(plots_logo,path=here("plots","team"))
}
{
  lapply(list.files(here("R","plot","league-plots"), full.names=TRUE), source, encoding="utf-8")
  plots <- list()
  plots$goals_xg_player <- goals_xg_player(season)
  plots$xg_xa_player <- xg_xa_player(season)
  plots$progressive_carries_player <- progressive_carries_player(season)
  plots$xg_team <- xg_team(season,per90=FALSE)
  plots$xg_team_p90 <- xg_team(season,per90=TRUE)
  plots$xg_team_scatter <- xg_team_scatter(season,per90=FALSE)
  plots$xg_team_scatter_p90 <- xg_team_scatter(season,per90=TRUE)
  plots$psxg_for <- psxg_for(season)
  plots$psxg_against <- psxg_against(season)
  
  plots_logo <-
    plots %>%
    add_logo(path=here("images","SB_Regular.png"),x=1,y=1,hjust=1.1,width=0.2) %>%
    add_logo(path=here("images","fbref.png"),x=0.88,y=1,hjust=1.1,width=0.29)
  
  save_plots(plots_logo,path=here("plots","league"))
}
