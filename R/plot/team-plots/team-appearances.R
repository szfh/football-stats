team_appearances <- function(team,season,since=NA){
  positions <-
    data$fbref$match_lineups %>%
    {if (!is.na(since)) filter(., Matchday>=as.Date(since)) else .} %>%
    select(Player=Player_Name,Pos,Min) %>%
    mutate(Pos=str_sub(Pos,1,2)) %>%
    filter(!is.na(Pos)) %>%
    mutate(Pos=case_when(
      Pos=="LB" ~ "FB",
      Pos=="RB" ~ "FB",
      Pos=="WB" ~ "FB",
      Pos=="DM" ~ "CM",
      Pos=="LM" ~ "AM",
      Pos=="RM" ~ "AM",
      Pos=="LW" ~ "AM",
      Pos=="RW" ~ "AM",
      Pos=="FW" ~ "ST",
      TRUE ~ Pos
    )) %>%
    mutate(Pos=factor(Pos,levels=c("GK","CB","FB","CM","AM","ST"))) %>%
    # mutate(Pos=factor(Pos,levels=c("GK","CB","LB","RB","DM","CM","LM","RM","FW")))
    group_by(Player,Pos) %>%
    summarise(Min=sum(Min,na.rm=TRUE),.groups="drop") %>%
    group_by(Player) %>%
    slice_max(Min, with_ties=FALSE) %>%
    ungroup() %>%
    select(Player,Pos)
  
  plot <-
    data$fbref$advanced_stats_player_summary %>%
    filter(season %in% !!season) %>%
    filter(Team %in% !!team) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"ymd")) %>%
    {if (!is.na(since)) filter(., Match_Date>=as.Date(since)) else .} %>%
    mutate(
      Home_Team=shorten_team_names(Home_Team),
      Away_Team=shorten_team_names(Away_Team)
    ) %>%
    mutate(
      Opposition=ifelse(Home_Away=="Home",Away_Team,Home_Team),
      Team_Score=ifelse(Home_Away=="Home",Home_Score,Away_Score),
      Opposition_Score=ifelse(Home_Away=="Home",Away_Score,Home_Score),
      .after="Team"
    ) %>%
    mutate(Home_Away_Short=ifelse(Home_Away=="Home","H","A")) %>%
    mutate(Match=glue("{Opposition} {Home_Away_Short} {Team_Score}-{Opposition_Score}")) %>%
    mutate(Match=reorder_within(Match, Match_Date, season)) %>%
    select(season,Match_Date,Match,Player,Min) %>%
    group_by(Player) %>%
    mutate(Min_Total=sum(Min)) %>%
    ungroup() %>%
    arrange(desc(Min_Total)) %>%
    left_join(positions) %>%
    mutate(Player=fct_reorder(Player,Min_Total)) %>%
    ggplot(aes(x=Match,y=Player)) +
    geom_tile(aes(alpha=Min),fill="darkred") +
    geom_text(aes(label=Min),color="black",size=1.6) +
    facet_grid(Pos ~ season, scales="free", space="free") +
    theme[["solar"]]() +
    theme(
      axis.text.x=element_text(size=4,angle=45,hjust=1),
      axis.text.y=element_text(size=4),
      plot.title=element_markdown(),
      plot.caption=element_text(size=5),
      strip.text.x=element_blank(),
      # strip.text.y=element_text(size=6,angle=0)
      strip.text.y=element_blank()
    ) +
    labs(
      title=glue("{shorten_team_names(team)} minutes played"),
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_reordered() +
    scale_y_reordered() +
    scale_alpha_continuous(range=c(0.1,0.7))
  
  return(plot)
}
