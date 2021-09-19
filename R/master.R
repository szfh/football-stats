# libraries
# source(here::here("R","library.R"),encoding="utf-8")

# raw
# source(here("R","raw","raw-fbref.R"),encoding="utf-8")
# scrape_fbref()
# source(here("R","raw","delete-recent.R"),encoding="utf-8")
# delete_recent(days=7)
# source(here("R","raw","raw-understat.R"),encoding="utf-8")
# scrape_understat()
# source(here("R","raw","raw-canpl.R"),encoding="utf-8")
# import_canpl()
# source(here("R","raw","raw-fivethirtyeight.R"),encoding="utf-8")
# import_fivethirtyeight()

# join
# source(here("R","join","join.R"),encoding="utf-8")
# data <- join()

# plot
# source(here("R","themes.R"),encoding="utf-8")
# source(here("R","plot","plot-team.R"),encoding="utf-8")
# plot_team(data=join(),season=c("2020-2021","2021-2022"))
# plot_team(data=join(),season=c("2018-2019","2019-2020","2020-2021","2021-2022"))
# source(here("R","plot","plot-league.R"),encoding="utf-8")
# plot_league(data=join())
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
