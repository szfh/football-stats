shots_trend <- function(team,season,lastn=NA,since=NA){
  
  plot <-
    data$fbref$advanced_stats_team_summary %>%
    filter(Team %in% !!team) %>%
    filter(!is.na(Sh)) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"ymd")) %>%
    select(season,Match_Date,Team,Home_Away,Home_Team,Home_Score,Away_Team,Away_Score,Sh) %>%
    arrange(Match_Date) %>%
    mutate(
      Home_Team=shorten_team_names(Home_Team),
      Away_Team=shorten_team_names(Away_Team)
    ) %>%
    mutate(Opposition=ifelse(Home_Away=="Home",Away_Team,Home_Team),.after="Team") %>%
    mutate(
      Team_Score=ifelse(Home_Away=="Home",Home_Score,Away_Score),
      Opposition_Score=ifelse(Home_Away=="Home",Away_Score,Home_Score),
    ) %>%
    mutate(
      Sh_mva=get_mva(Sh)
    ) %>%
    filter(season %in% !!season) %>%
    mutate(season=case_when(
      season==2019 & Match_Date < as.Date("2022-04-01")~ 2019.1,
      season==2019 ~ 2019.2,
      season==2023 & Match_Date < as.Date("2022-12-01")~ 2023.1,
      season==2023 ~ 2023.2,
      TRUE ~ season)) %>%
    {if (!is.na(since)) filter(., Match_Date>=as.Date(since)) else .} %>%
    {if (!is.na(lastn)) slice_tail(., n=lastn) else .} %>%
    mutate(Home_Away_Short=ifelse(Home_Away=="Home","H","A"),.after="Home_Away") %>%
    mutate(Match=glue::glue("{Opposition} {Home_Away_Short} {Team_Score}-{Opposition_Score}")) %>%
    mutate(Match=reorder_within(Match, Match_Date, season)) %>%
    ggplot(aes(x=Match)) +
    geom_point(aes(y=Sh),size=1,colour="darkred",fill="darkred",alpha=0.5,shape=23) +
    geom_line(aes(y=Sh_mva,group=season),colour="darkred",linetype="longdash",size=0.7) +
    theme[["solar"]]() +
    theme(
      axis.text.x=element_text(size=6,angle=60,hjust=1),
      axis.title.y=element_blank(),
      axis.text.y=element_text(),
      plot.title=element_markdown(),
      plot.caption=element_text(),
      strip.text=element_blank()
    ) +
    labs(
      title=glue("{shorten_team_names(team)} <b style='color:darkred'>shots</b>"),
      x=element_blank(),
      y=glue("Expected goals <b style='color:darkred'>for</b> / <b style='color:royalblue'>against</b>")
    ) +
    scale_x_reordered() +
    scale_y_continuous(limits=c(0,NA),breaks=seq(0,100,2),expand=expansion(add=c(0,0.8))) +
    facet_grid(cols=vars(season), space="free", scales="free_x")
  
  return(plot)
}