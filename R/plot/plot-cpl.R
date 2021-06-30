source(here("R","plot","plot-utils.R"))

plot_cpl <- function(data,season=2021,team="all"){
  plots <- list()
  
  plots$goals_xg <-
    data$canpl$team_total %>%
    filter(Season==!!season) %>%
    select(Team,Team_ID=optaTeamId,Goal,PenGoal,NPxG=NonPenxG) %>%
    mutate(Team_ID=as.character(Team_ID)) %>%
    mutate(NPG=Goal-PenGoal) %>%
    ggplot(aes(x=NPxG,y=NPG)) +
    geom_point(aes(fill=Team_ID),colour="black",shape=23,size=3,position=position_jitter(width=0.2,height=0.05,seed=999)) +
    geom_text_repel(aes(label=Team),size=4,position=position_jitter(width=0.2,height=0.05,seed=999)) +
    theme[["solar"]]() +
    labs(
      title=glue("Canadian Premier League Goals/xG {season}"),
      x="Expected Goals",
      y="Goals scored (non-penalties)") +
    scale_fill_manual(values=palette[["cpl2"]]())
  
  plots$xg <-
    data$canpl$team_total %>%
    filter(Season==!!season) %>%
    select(Team,Team_ID=optaTeamId,Goal,PenGoal,xG=ExpG,NPxG=NonPenxG,xGA=ExpGAg) %>%
    mutate(Team_ID=as.character(Team_ID)) %>%
    mutate(NPG=Goal-PenGoal) %>%
    ggplot(aes(x=xG,y=xGA)) +
    geom_point(aes(fill=Team_ID),colour="black",shape=23,size=4,alpha=0.8,position=position_jitter(width=0.1,height=0.1,seed=999)) +
    geom_text_repel(aes(label=Team),size=4,position=position_jitter(width=0.1,height=0.1,seed=999)) +
    theme[["solar"]]() +
    labs(
      title=glue("Canadian Premier League xG {season}"),
      x="Expected Goals",
      y="xG A") +
    scale_x_continuous(breaks=seq(0,100,0.5),expand=expansion(add=c(0.5))) +
    scale_y_reverse(breaks=seq(0,100,0.5),expand=expansion(add=c(0.5))) +
    scale_fill_manual(values=palette[["cpl2"]]())
  
  plots$xg90 <-
    data$canpl$team_total %>%
    filter(Season==!!season) %>%
    select(Team,Team_ID=optaTeamId,MP=GM,Goal,PenGoal,xG=ExpG,NPxG=NonPenxG,xGA=ExpGAg) %>%
    mutate(Team_ID=as.character(Team_ID)) %>%
    mutate(NPG=Goal-PenGoal) %>%
    mutate(
      xG90=xG/MP,
      npxG90=NPxG/MP,
      xGA90=xGA/MP
    ) %>%
    ggplot(aes(x=xG90,y=xGA90)) +
    geom_point(aes(fill=Team_ID),colour="black",shape=23,size=4,alpha=0.8,position=position_jitter(width=0.1,height=0.1,seed=999)) +
    geom_text_repel(aes(label=Team),size=4,position=position_jitter(width=0.1,height=0.1,seed=999)) +
    theme[["solar"]]() +
    labs(
      title=glue("Canadian Premier League xG {season}"),
      x="xG per match",
      y="xG against per match") +
    scale_x_continuous(breaks=seq(0,100,0.5),expand=expansion(add=c(0.5))) +
    scale_y_reverse(breaks=seq(0,100,0.5),expand=expansion(add=c(0.5))) +
    scale_fill_manual(values=palette[["cpl2"]]())
  
  plots$xgtrend <-
    data$canpl$team_match %>%
    # filter(Season==!!season) %>%
    filter(Team==!!team) %>%
    select(Season,Team,Date,xG=ExpG,xGA=ExpGAg,scatterExtra) %>%
    mutate(Date=as.Date(Date)) %>%
    mutate(Season=case_when(
      Date<lubridate::as_date("2019-7-2") ~ "2019\nSpring Season",
      Date<lubridate::as_date("2019-10-20") ~ "2019\nFall Season",
      TRUE ~ "2020\nIsland Games")) %>%
    mutate(Game=reorder_within(scatterExtra, Date, Season)) %>%
    ggplot(aes(x=Game)) +
    geom_point(aes(y=xG),size=1,colour="darkred",fill="darkred",alpha=0.5,shape=23) +
    geom_spline(aes(y=xG,group=Season),spar=0.5,colour="darkred",linetype="longdash",size=0.7) +
    geom_point(aes(y=xGA),size=1,colour="royalblue",fill="royalblue",alpha=0.5,shape=23) +
    geom_spline(aes(y=xGA,group=Season),spar=0.5,colour="royalblue",linetype="longdash",size=0.7) +
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
    facet_grid(cols=vars(Season), space="free", scales="free_x")
  
  plots$xgsegment <-
    data$canpl$team_match %>%
    # filter(Season==!!season) %>%
    filter(Team==!!team) %>%
    select(Season,Team,Date,xG=ExpG,xGA=ExpGAg,scatterExtra) %>%
    mutate(Date=as.Date(Date)) %>%
    mutate(Season=case_when(
      Date<lubridate::as_date("2019-7-2") ~ "2019\nSpring Season",
      Date<lubridate::as_date("2019-10-20") ~ "2019\nFall Season",
      TRUE ~ "2020\nIsland Games")) %>%
    mutate(Game=reorder_within(scatterExtra, Date, Season)) %>%
    ggplot(aes(y=Game)) +
    geom_segment(aes(x=0,xend=xG,y=Game,yend=Game),colour=colour[["sfc"]][["light"]],size=2.5) +
    geom_segment(aes(x=0,xend=-xGA,y=Game,yend=Game),colour=colour[["medium"]][[1]],size=2.5) +
    theme[["solar"]]() +
    theme(
      plot.title=element_markdown(size=12),
      axis.text.x=element_text(),
      axis.text.y=element_text(size=6),
      strip.text=element_blank()
    ) +
    labs(
      title=glue("<b style='color:#265DAB'>Opposition xG</b> | <b style='color:#D71920'>{team} xG</b>"),
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(breaks=seq(-10,10,1),labels=abs(seq(-10,10,1)),expand=expansion(add=c(0.1,1))) +
    scale_y_reordered() +
    facet_grid(rows=vars(Season), space="free", scales="free_y")
  
  plots$squadxg <-
    data$canpl$team_total %>%
    filter(Season==!!season) %>%
    select(Team,Team_ID=optaTeamId,Goal,PenGoal,xG=ExpG,NPxG=NonPenxG,xGA=ExpGAg) %>%
    mutate(Team_ID=as.character(Team_ID)) %>%
    mutate(xGA=-xGA) %>%
    make_long_data(levels=c("xG","xGA"),labels=c("Expected Goals For","Expected Goals Against")) %>%
    ggplot(aes(x=0,y=n)) +
    geom_text_repel(
      aes(label=Team),
      size=3.5,
      nudge_x=0.5,
      direction="y",
      hjust=0,
      segment.size=0.4,
      segment.alpha=0.8,
      box.padding=0.05,
      fontface="bold"
    ) +
    geom_point(aes(fill=Team_ID),size=3.5,shape=23,colour="black") +
    theme[["solarfacet"]]() +
    facet_wrap("key",scales="free") +
    theme(
      plot.title=element_markdown(),
      strip.text.x=element_markdown()
    ) +
    labs(
      title=glue("{season} Canadian Premier League xG"),
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(limit=c(0,1)) +
    scale_y_continuous(breaks=seq(-100,100,1),labels=abs(seq(-100,100,1)),expand=expansion(add=c(0.5))) +
    scale_fill_manual(values=palette[["cpl2"]]())
  
  plots_logo <- add_logo(plots,here("images","StatsPerformLogo.png"),x=0.9,y=1)
  plots_logo <- add_logo(plots_logo,here("images","CPL.png"),x=1,y=1)
  
  save_plots(plots_logo,path=here("plots","CPL"))
}
