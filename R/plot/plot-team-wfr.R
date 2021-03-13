source(here("R","plot","plot-utils.R"))

plot_team_wfr <- function(data,team="Southampton",season="2020-2021"){

  force(data)
  plots <- list()
  
  browser()
  
  plots$xgxa <-
    data$fbref$player_advanced_stats_match %>%
    filter(Season %in% !!season) %>%
    filter(Team %in% !!team) %>%
    select(Player,Min,npxG=npxG_Expected,xA) %>%
    group_by(Player) %>%
    summarise(across(where(is.numeric),sum)) %>%
    mutate(Focus=case_when(
      npxG==0 & xA==0 ~ FALSE,
      min_rank(desc(npxG))<=8 ~ TRUE,
      min_rank(desc(xA))<=8 ~ TRUE,
      TRUE ~ FALSE)) %>%
    glimpse %>%
    ggplot(aes(x=npxG,y=xA)) +
    geom_point(aes(fill=Focus),shape=23,size=2.5,alpha=0.8,colour=colour[["sfc"]][["black"]]) +
    geom_text_repel(aes(label=ifelse(Focus,Player,"")),size=rel(2.5)) +
    theme[["solar"]]() +
    labs(
      title=glue("{team} xG/xA"),
      x="Expected goals (penalties excluded)",
      y="Expected assists"
    ) +
    scale_x_continuous(limits=c(0,NA),breaks=seq(0,30,1),expand=expansion(add=c(0,0.2))) +
    scale_y_continuous(limits=c(0,NA),breaks=seq(0,30,1),expand=expansion(add=c(0,0.2))) +
    scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["main"]],"FALSE"=colour[["sfc"]][["grey"]]))
  
  plots_logo <-
    plots %>%
    add_logo(path=here("images","SB_Regular.png"),x=1,y=1,hjust=1.1,width=0.2) %>%
    add_logo(path=here("images","fbref.png"),x=0.88,y=1,hjust=1.1,width=0.29)

  save_plots(plots_logo,path=here("plots","team_wfr"))
  
}