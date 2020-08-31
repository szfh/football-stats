source(here("R","plot","plot-utils.R"))

plot_cpl <- function(data){
  plots <- list()
  
  plots_wm <- add_watermark_cpl(plots,here("images","StatsPerformLogo.png"),x=0.9,y=1)
  plots_wm <- add_watermark_cpl(plots_wm,here("images","CPL.png"),x=1,y=1)
  
  save_plots(plots_wm,path=here("plots","CPL"))
}
