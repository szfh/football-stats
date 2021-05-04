source(here("R","plot","plot-utils.R"),encoding="utf-8")
source(here("R","themes.R"),encoding="utf-8")

plot_league_wfr <- function(data,season="2020-2021"){
  
  force(data)
  plots <- list()
  # browser()
  season <- expand_seasons(season)
  
  
  
  plots_logo <-
    plots %>%
    add_logo(path=here("images","SB_Regular.png"),x=1,y=1,hjust=1.1,width=0.2) %>%
    add_logo(path=here("images","fbref.png"),x=0.88,y=1,hjust=1.1,width=0.29)
  
  save_plots(plots_logo,path=here("plots","league_wfr"))
  
}