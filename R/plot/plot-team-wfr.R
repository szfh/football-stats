source(here("R","plot","plot-utils.R"),encoding="utf-8")
source(here("R","themes.R"),encoding="utf-8")

plot_team_wfr <- function(data,team="Southampton",season="2020-2021"){
  
  force(data)
  plots <- list()
  
  penalties <-
    data$fbref$team_advanced_stats_match %>%
    filter((Home_Team %in% !!team)|(Away_Team %in% !!team)) %>%
    select(Match_Date,Home_Away,PKatt) %>%
    unique() %>%
    pivot_wider(names_from=Home_Away, values_from=PKatt, names_glue="PK_{Home_Away}")
  
  plots$xgtrendmva <-
    data$fbref$team_advanced_stats_match %>%
    filter(Team %in% !!team) %>%
    select(Season,Match_Date,Team,Home_Team,Home_Score,Home_xG,Away_Team,Away_Score,Away_xG,Home_Away,PKatt) %>%
    left_join(penalties) %>%
    mutate(Home_npxG=Home_xG-(PK_Home*0.7)) %>%
    mutate(Away_npxG=Away_xG-(PK_Away*0.7)) %>%
    make_for_against_matches() %>%
    select(-Home_Team,-Home_Score,-Home_xG,-Away_Team,-Away_Score,-Away_xG) %>%
    mutate(Match_Date=lubridate::parse_date_time(Match_Date,"mdy")) %>%
    arrange(Match_Date) %>%
    mutate(Team_npxG_mva=get_mva(Team_npxG)) %>%
    mutate(Opposition_npxG_mva=get_mva(Opposition_npxG)) %>%
    filter(Season %in% !!season) %>%
    # filter(date>=as.Date("2020-01-01")) %>%
    mutate(Season=case_when(
      Match_Date>=as.Date("2019-08-01") & Match_Date<as.Date("2020-04-01") ~ "2019-20 part 1",
      Match_Date>=as.Date("2020-04-01") & Match_Date<as.Date("2020-08-01") ~ "2019-20 part 2",
      TRUE ~ Season)) %>%
    mutate(Home_Away_Short=ifelse(Home_Away=="Home","H","A")) %>%
    mutate(Match=glue::glue("{Opposition} {Home_Away_Short} {Team_Score}-{Opposition_Score}")) %>%
    mutate(Match=reorder_within(Match, Match_Date, Season)) %>%
    ggplot(aes(x=Match)) +
    geom_point(aes(y=Team_npxG),size=1,colour="darkred",fill="darkred",alpha=0.5,shape=23) +
    geom_line(aes(y=Team_npxG_mva,group=Season),colour="darkred",linetype="longdash",size=0.7) +
    geom_point(aes(y=Opposition_npxG),size=1,colour="royalblue",fill="royalblue",alpha=0.5,shape=23) +
    geom_line(aes(y=Opposition_npxG_mva,group=Season),colour="royalblue",linetype="longdash",size=0.7) +
    theme[["solar"]]() +
    theme(
      axis.text.x=element_text(size=6,angle=60,hjust=1),
      axis.title.y=element_markdown(),
      axis.text.y=element_text(),
      plot.title=element_markdown(),
      plot.caption=element_text(),
      strip.text=element_blank()
    ) +
    labs(
      title=glue("{team} <b style='color:darkred'>attack</b> / <b style='color:royalblue'>defence</b> xG trend"),
      x=element_blank(),
      y=glue("Expected goals <b style='color:darkred'>for</b> / <b style='color:royalblue'>against</b>")
    ) +
    scale_x_reordered() +
    scale_y_continuous(limits=c(0,NA),expand=expansion(add=c(0,0.1))) +
    facet_grid(cols=vars(Season), space="free", scales="free_x")
  
  starting <-
    data$fbref$match_lineups %>%
    separate(Matchday,c("Match","Match_Date")," – ") %>%
    select(Match_Date,"Player"=Player_Name,Starting)
  
  plots$minutes <-
    data$fbref$player_advanced_stats_match %>%
    filter(Season %in% !!season) %>%
    filter(Team %in% !!team) %>%
    select(Match_Date,Player,Min) %>%
    left_join(starting) %>%
    select(-Match_Date) %>%
    group_by(Player,Starting) %>%
    summarise(across(where(is.numeric),sum)) %>%
    ungroup() %>%
    pivot_wider(names_from=Starting, values_from=Min, names_glue="Min_{Starting}") %>%
    mutate(Min_Pitch=replace_na(Min_Pitch,0)) %>%
    mutate(Min_Bench=replace_na(Min_Bench,0)) %>%
    mutate(Min_Total=Min_Pitch+Min_Bench) %>%
    mutate(Player=fct_reorder(Player,Min_Total)) %>%
    ggplot(aes(y=Player)) +
    geom_segment(aes(y=Player,yend=Player,x=0,xend=Min_Pitch),colour=colour[["sfc"]][["main"]],size=2.5,alpha=0.8) +
    geom_segment(aes(y=Player,yend=Player,x=Min_Pitch,xend=Min_Total),colour=colour[["sfc"]][["light"]],size=2.5,alpha=0.8) +
    theme[["solar"]]() +
    theme(
      plot.title=element_markdown(),
      axis.line=element_blank(),
      axis.text=element_text(size=7),
      strip.text.y=element_text(angle=0)
    ) +
    labs(
      title=glue("League minutes<br />(<b style='color:#D71920'>from start</b> | <b style='color:#ED5C5C'>from bench</b>)"),
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(breaks=seq(0,90*38*3,180),expand=expansion(add=c(0,20)))
  
  plots$xgxa <-
    data$fbref$player_advanced_stats_match %>%
    filter(Season %in% !!season) %>%
    filter(Team %in% !!team) %>%
    select(Player,Min,npxG=npxG_Expected,xA=xA_Expected) %>%
    group_by(Player) %>%
    summarise(across(where(is.numeric),sum)) %>%
    ungroup() %>%
    mutate(Focus=case_when(
      npxG==0 & xA==0 ~ FALSE,
      min_rank(desc(npxG))<=8 ~ TRUE,
      min_rank(desc(xA))<=8 ~ TRUE,
      TRUE ~ FALSE)) %>%
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
  
  plots$psxg_against <-
    data$fbref$team_advanced_stats_match %>%
    filter(Season %in% !!season) %>%
    select(Team,Match_Date,GA=GA_Shot,psxG=PSxG_Shot) %>%
    mutate(Match_Date=lubridate::parse_date_time(Match_Date,"mdy")) %>%
    mutate(psxGD=psxG-GA) %>%
    arrange(Match_Date) %>%
    group_by(Team) %>%
    mutate(cumulative_psxGD=cumsum(psxGD)) %>%
    ungroup() %>%
    mutate(focus=ifelse(Team=="Southampton",TRUE,FALSE)) %>%
    ggplot(aes(x=Match_Date,y=cumulative_psxGD)) +
    geom_path(aes(group=Team,alpha=focus,colour=focus),size=0.75) +
    theme[["solar"]]() +
    theme(
      axis.text.x=element_markdown(),
      axis.title.y=element_markdown(),
      axis.text.y=element_text(),
      plot.title=element_markdown(),
      plot.caption=element_text(),
      strip.text=element_blank()
    ) +
    labs(
      title="GK expected saves - <b style='color:darkred'>Southampton</b>",
      x=element_blank(),
      y="Post-shot xG performance"
    ) +
    scale_x_datetime(expand=expansion(add=100)) +
    scale_y_continuous(expand=expansion(add=0.2)) +
    scale_colour_manual(values=c("TRUE"="darkred","FALSE"="darkgray")) +
    scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.5))
  
  # # plots$xgtrendmva <-
  #   data$fbref$matches %>%
  #   # make_long_matches() %>%
  #   filter(squad %in% !!team) %>%
  #   mutate(xgf_mva=get_mva(xgf)) %>%
  #   mutate(xga_mva=get_mva(xga)) %>%
  #   filter(season %in% !!season) %>%
  #   # filter(date>=as.Date("2020-01-01")) %>%
  #   mutate(season=case_when(
  #     date>as.Date("2019-08-01") & date<as.Date("2020-03-31") ~ "2019-20 part 1",
  #     date>as.Date("2020-04-01") & date<as.Date("2020-07-30") ~ "2019-20 part 2",
  #     TRUE ~ season)) %>%
  #   filter(!is.na(homegls)) %>%
  #   mutate(shortha=ifelse(ha=="home","H","A")) %>%
  #   mutate(match=glue::glue("{opposition} {shortha} {glsf}-{glsa}")) %>%
  #   mutate(match=reorder_within(match, date, season)) %>%
  #   ggplot(aes(x=match)) +
  #   geom_point(aes(y=xgf),size=1,colour="darkred",fill="darkred",alpha=0.5,shape=23) +
  #   geom_line(aes(y=xgf_mva,group=season),colour="darkred",linetype="longdash",size=0.7) +
  #   geom_point(aes(y=xga),size=1,colour="royalblue",fill="royalblue",alpha=0.5,shape=23) +
  #   geom_line(aes(y=xga_mva,group=season),colour="royalblue",linetype="longdash",size=0.7) +
  #   theme[["solar"]]() +
  #   theme(
  #     axis.text.x=element_text(size=6,angle=60,hjust=1),
  #     axis.title.y=element_markdown(),
  #     axis.text.y=element_text(),
  #     plot.title=element_markdown(),
  #     plot.caption=element_text(),
  #     strip.text=element_blank()
  #   ) +
  #   labs(
  #     title=glue("{squad} <b style='color:darkred'>attack</b> / <b style='color:royalblue'>defence</b> xG trend"),
  #     x=element_blank(),
  #     y=glue("Expected goals <b style='color:darkred'>for</b> / <b style='color:royalblue'>against</b>")
  #   ) +
  #   scale_x_reordered() +
  #   scale_y_continuous(limits=c(0,NA),expand=expansion(add=c(0,0.1))) +
  #   facet_grid(cols=vars(season), space="free", scales="free_x")
  
  # plots$xgsegment <-
  #   data$fbref$matches %>%
  #   make_long_matches() %>%
  #   filter(season %in% !!season) %>%
  #   filter(squad %in% !!squad) %>%
  #   # filter(date>=as.Date("2020-01-01")) %>%
  #   mutate(season=case_when(
  #     date>as.Date("2019-08-01") & date<as.Date("2020-03-31") ~ "2019-20 part 1",
  #     date>as.Date("2020-04-01") & date<as.Date("2020-07-30") ~ "2019-20 part 2",
  #     TRUE ~ season)) %>%
  #   filter(!is.na(homegls)) %>%
  #   mutate(shortha=ifelse(ha=="home","H","A")) %>%
  #   mutate(match=glue("{opposition} {shortha} {glsf}-{glsa}")) %>%
  #   mutate(match=reorder_within(match, desc(date), season)) %>%
  #   ggplot(aes(y=match)) +
  #   geom_segment(aes(x=0,xend=xgf,y=match,yend=match),colour=colour[["sfc"]][["light"]],size=2) +
  #   geom_segment(aes(x=0,xend=-xga,y=match,yend=match),colour=colour[["medium"]][[1]],size=2) +
  #   theme[["solar"]]() +
  #   theme(
  #     plot.title=element_markdown(),
  #     axis.text.x=element_text(),
  #     axis.text.y=element_text(size=6),
  #     strip.text=element_blank()
  #   ) +
  #   labs(
  #     title=glue("<b style='color:#265DAB'>Opposition xG</b> | <b style='color:#D71920'>{squad} xG</b>"),
  #     x=element_blank(),
  #     y=element_blank()
  #   ) +
  #   scale_x_continuous(breaks=seq(-10,10,1),labels=abs(seq(-10,10,1)),expand=expansion(add=c(0.1,1))) +
  #   scale_y_reordered() +
  #   facet_grid(rows=vars(season), space="free", scales="free_y")
  
  plots_logo <-
    plots %>%
    add_logo(path=here("images","SB_Regular.png"),x=1,y=1,hjust=1.1,width=0.2) %>%
    add_logo(path=here("images","fbref.png"),x=0.88,y=1,hjust=1.1,width=0.29)
  
  save_plots(plots_logo,path=here("plots","team_wfr"))
  
}