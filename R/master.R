# libraries
# source(here::here("R","library.R"),encoding="utf-8")

# raw
# source(here("R","raw","raw-fbref.R"),encoding="utf-8")
# scrape_fbref()
# source(here("R","raw","raw-fbref-wfr.R"),encoding="utf-8")
# scrape_fbref_wfr(save_path=here("data","fbref-wfr.rds"),current_season=2022)
# source(here("R","raw","raw-understat.R"),encoding="utf-8")
# scrape_understat()
# source(here("R","raw","raw-canpl.R"),encoding="utf-8")
# import_canpl()

# plot
# source(here("R","join","join.R"),encoding="utf-8")
# source(here("R","join","join-wfr.R"),encoding="utf-8")
# j1 <- join_wfr(save_path_fbref=here("data","fbref-wfr.rds"))
# source(here("R","themes.R"),encoding="utf-8")
# source(here("R","plot","plot-team.R"),encoding="utf-8")
# plot_team(data=join())
# source(here("R","plot","plot-league.R"),encoding="utf-8")
# plot_league(data=join())
# source(here("R","plot","plot-team-wfr.R"),encoding="utf-8")
# plot_team_wfr(data=join_wfr(save_path_fbref=here("data","fbref-wfr.rds")))
# source(here("R","plot","plot-league-wfr.R"),encoding="utf-8")
# plot_league_wfr(data=join_wfr(save_path_fbref=here("data","fbref-wfr.rds")))
# source(here("R","plot","plot-cpl.R"),encoding="utf-8")
# plot_cpl(data=join()["canpl"])

# knit markdown
# knitr::knit(here::here("R","README.Rmd"))

# old
# source(here("R","master-utils.R"))
# source(here("R","raw","understat-static.R"))
# source(here("R","raw","fivethirtyeight.R"))
# source(here("R","tidy","tidy-fbref.R"))
# source(here("R","tidy","fivethirtyeight.R"))
