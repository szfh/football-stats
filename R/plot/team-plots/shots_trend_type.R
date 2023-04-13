shots_trend_type <- function(team,season,lastn=NA,since=NA){
  
  get_shot_type <- function(data){
    data <-
      data %>%
      mutate(Shot_Type=case_when(
        str_detect(Player,"(pen)") ~ "SP",
        Notes=="Free kick" ~ "SP",
        Notes=="Penalty" ~ "SP",
        Event_SCA_1=="Pass (Live)" ~ "OP",
        Event_SCA_1=="Pass (Dead)" ~ "SP",
        Event_SCA_2=="Pass (Live)" ~ "OP",
        Event_SCA_2=="Pass (Dead)" ~ "SP",
        # SCA1_Event=="Pass (Live)" ~ "OP",
        # SCA2_Event=="Pass (Live)" ~ "OP",
        .default="OP"
      ))
    return(data)
  }  
  
  shots <-
    data$fbref$match_shots %>%
    rename(Team=Squad,Match_Date=Date) %>%
    filter(Team %in% !!team) %>%
    # filter(season %in% !!season) %>%
    get_shot_type() %>%
    select(season,Match_Date,Team,Home_Away,Shot_Type) %>%
    group_by(season,Match_Date,Team,Home_Away,Shot_Type) %>%
    summarise(Shots=n()) %>%
    ungroup() %>%
    mutate(Match_Date=parse_date_time(Match_Date,"ymd"))
  
  matches <-
    data$fbref$advanced_stats_team_summary %>%
    filter(Team %in% !!team) %>%
    # filter((!is.na(Home_xG))|!is.na(Away_xG)) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"ymd")) %>%
    select(season,Match_Date,Team,Home_Team,Home_Score,Home_xG,Away_Team,Away_Score,Away_xG,Home_Away)
  
  plot <-
    inner_join(matches,shots) %>%
    pivot_wider(
      names_from=Shot_Type,
      values_from=Shots,
      values_fill=0
    ) %>%
    arrange(Match_Date) %>%
    mutate(
      OP_mva=get_mva(OP),
      SP_mva=get_mva(SP)
    ) %>%
    filter(season %in% !!season) %>%
    mutate(season=case_when(
      season==2019 & Match_Date < as.Date("2022-04-01")~ 2019.1,
      season==2019 ~ 2019.2,
      season==2023 & Match_Date < as.Date("2022-12-01")~ 2023.1,
      season==2023 ~ 2023.2,
      TRUE ~ season)) %>%
    mutate(
      Home_Team=shorten_team_names(Home_Team),
      Away_Team=shorten_team_names(Away_Team)
    ) %>%
    mutate(Opposition=ifelse(Home_Away=="Home",Away_Team,Home_Team),.after="Team") %>%
    mutate(
      Team_Score=ifelse(Home_Away=="Home",Home_Score,Away_Score),
      Opposition_Score=ifelse(Home_Away=="Home",Away_Score,Home_Score)
    ) %>%
    mutate(Opposition=ifelse(Home_Away=="Home",Away_Team,Home_Team),.after="Team") %>%
    {if (!is.na(since)) filter(., Match_Date>=as.Date(since)) else .} %>%
    {if (!is.na(lastn)) slice_tail(., n=lastn) else .} %>%
    mutate(Home_Away_Short=ifelse(Home_Away=="Home","H","A"),.after="Home_Away") %>%
    mutate(Match=glue::glue("{Opposition} {Home_Away_Short} {Team_Score}-{Opposition_Score}")) %>%
    mutate(Match=reorder_within(Match, Match_Date, season)) %>%
    ggplot(aes(x=Match)) +
    geom_point(aes(y=OP),size=1,colour="darkred",fill="darkred",alpha=0.5,shape=23) +
    geom_line(aes(y=OP_mva,group=season),colour="darkred",linetype="longdash",size=0.7) +
    geom_point(aes(y=SP),size=1,colour="royalblue",fill="royalblue",alpha=0.5,shape=23) +
    geom_line(aes(y=SP_mva,group=season),colour="royalblue",linetype="longdash",size=0.7) +
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
      title=glue("{shorten_team_names(team)} shots from <b style='color:darkred'>open play</b> and <b style='color:royalblue'>set pieces</b>"),
      x=element_blank(),
      y=glue("Shots from <b style='color:darkred'>open play</b> and <b style='color:royalblue'>set pieces</b>")
    ) +
    scale_x_reordered() +
    scale_y_continuous(limits=c(0,NA),breaks=seq(0,100,2),expand=expansion(add=c(0.4,0.4))) +
    facet_grid(cols=vars(season), space="free", scales="free_x")
  
  return(plot)
}