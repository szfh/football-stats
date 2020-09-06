source(here("R","plot","plot-utils.R"))

plot_cpl <- function(data,team="all"){
  
  plots <- list()
  
  player20 <- data$canpl %>%
    slice(4) %>%
    select(data) %>%
    unnest(cols=data)
  
  team20 <- data$canpl %>%
    slice(8) %>%
    select(data) %>%
    unnest(cols=data)
  
  matches1920 <- data$canpl %>%
    slice(5,6) %>%
    select(name,data) %>%
    unnest(cols=data)
  
  plots$goalsxg <-
    team20 %>%
    select(team,team_id=optaTeamId,goal=Goal,pg=PenGoal,npxg=NonPenxG) %>%
    mutate(team_id=as.character(team_id)) %>%
    mutate(npg=goal-pg) %>%
    ggplot(aes(x=npxg,y=npg)) +
    geom_point(aes(fill=team_id),colour="black",shape=23,size=3,position=position_jitter(width=0.2,height=0.05,seed=999)) +
    geom_text_repel(aes(label=team),size=4,position=position_jitter(width=0.2,height=0.05,seed=999)) +
    theme[["solar"]]() +
    labs(
      title="Canadian Premier League Goals/xG 2020",
      x="Expected Goals",
      y="Goals scored (non-penalties)") +
    scale_fill_manual(values=palette[["cpl2"]]())
  
  plots$xg <-
    team20 %>%
    select(team,team_id=optaTeamId,goal=Goal,pg=PenGoal,xg=ExpG,npxg=NonPenxG,xga=ExpGAg) %>%
    mutate(team_id=as.character(team_id)) %>%
    mutate(npg=goal-pg) %>%
    ggplot(aes(x=xg,y=xga)) +
    geom_point(aes(fill=team_id),colour="black",shape=23,size=4,alpha=0.8,position=position_jitter(width=0.1,height=0.1,seed=999)) +
    geom_text_repel(aes(label=team),size=4,position=position_jitter(width=0.1,height=0.1,seed=999)) +
    theme[["solar"]]() +
    labs(
      title="Canadian Premier League xG 2020",
      x="Expected Goals",
      y="xG A") +
    scale_x_continuous(breaks=seq(0,100,2),expand=expansion(add=c(0.5))) +
    scale_y_reverse(breaks=seq(0,100,2),expand=expansion(add=c(0.5))) +
    scale_fill_manual(values=palette[["cpl2"]]())
  
  plots$xgtrend <-
    matches1920 %>%
    filter(team==!!team) %>%
    mutate(season=case_when(
      date<lubridate::as_date("2019-7-2") ~ "2019\nSpring Season",
      date<lubridate::as_date("2019-10-20") ~ "2019\nFall Season",
      TRUE ~ "2020\nIsland Games")) %>%
    mutate(season=factor(season,levels=c("2019\nSpring Season","2019\nFall Season","2020\nIsland Games"))) %>%
    mutate(game=reorder_within(scatterExtra, date, season)) %>%
    ggplot(aes(x=game)) +
    geom_point(aes(y=ExpG),size=1,colour="darkred",fill="darkred",alpha=0.5,shape=23) +
    geom_spline(aes(y=ExpG,group=season),spar=0.5,colour="darkred",linetype="longdash",size=0.7) +
    geom_point(aes(y=ExpGAg),size=1,colour="royalblue",fill="royalblue",alpha=0.5,shape=23) +
    geom_spline(aes(y=ExpGAg,group=season),spar=0.5,colour="royalblue",linetype="longdash",size=0.7) +
    theme[["solar"]]() +
    theme(
      axis.text.x=element_text(size=6,angle=60,hjust=1),
      axis.title.y=element_markdown(size=12),
      axis.text.y=element_text(),
      plot.title=element_markdown(),
      plot.caption=element_text(),
      # strip.text=element_blank()
      panel.grid.major.x=element_blank()
    ) +
    labs(
      title=glue("{team} <b style='color:darkred'>attack</b> / <b style='color:royalblue'>defence</b> xG trend"),
      x=element_blank(),
      y=glue("Expected goals <b style='color:darkred'>for</b> / <b style='color:royalblue'>against</b>")
    ) +
    scale_x_reordered() +
    scale_y_continuous(limits=c(0,NA),expand=expansion(add=c(0,0.1))) +
    facet_grid(cols=vars(season), space="free", scales="free_x")
  
  plots_logo <- add_logo(plots,here("images","StatsPerformLogo.png"),x=0.9,y=1)
  plots_logo <- add_logo(plots_logo,here("images","CPL.png"),x=1,y=1)
  
  save_plots(plots_logo,path=here("plots","CPL"))
}
