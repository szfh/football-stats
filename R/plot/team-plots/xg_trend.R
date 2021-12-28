xg_trend <- function(team,season,lastn=NA){
  penalties <-
    data$fbref$advanced_stats_team_summary %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    filter(!is.na(PKatt)) %>%
    filter((Home_Team %in% !!team)|(Away_Team %in% !!team)) %>%
    select(Match_Date,Home_Away,PKatt) %>%
    pivot_wider(names_from=Home_Away, values_from=PKatt, names_glue="PK_{Home_Away}")
  
  plot <-
    data$fbref$advanced_stats_team_summary %>%
    filter(Team %in% !!team) %>%
    filter((!is.na(Home_xG))|!is.na(Away_xG)) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    select(Season,Match_Date,Team,Home_Team,Home_Score,Home_xG,Away_Team,Away_Score,Away_xG,Home_Away) %>%
    left_join(penalties) %>%
    arrange(Match_Date) %>%
    mutate(
      Home_npxG=Home_xG-(PK_Home*0.7),
      Away_npxG=Away_xG-(PK_Away*0.7)
    ) %>%
    mutate(
      Home_Team=shorten_team_names(Home_Team),
      Away_Team=shorten_team_names(Away_Team)
    ) %>%
    mutate(Opposition=ifelse(Home_Away=="Home",Away_Team,Home_Team),.after="Team") %>%
    mutate(
      Team_Score=ifelse(Home_Away=="Home",Home_Score,Away_Score),
      Team_npxG=ifelse(Home_Away=="Home",Home_npxG,Away_npxG),
      Opposition_Score=ifelse(Home_Away=="Home",Away_Score,Home_Score),
      Opposition_npxG=ifelse(Home_Away=="Home",Away_npxG,Home_npxG)
    ) %>%
    mutate(
      Team_npxG_mva=get_mva(Team_npxG),
      Opposition_npxG_mva=get_mva(Opposition_npxG)) %>%
    filter(Season %in% !!season) %>%
    mutate(Season=case_when(
      Match_Date>=as.Date("2019-08-01") & Match_Date<as.Date("2020-04-01") ~ "2019-20 part 1",
      Match_Date>=as.Date("2020-04-01") & Match_Date<as.Date("2020-08-01") ~ "2019-20 part 2",
      TRUE ~ Season)) %>%
    # filter(Match_Date>=as.Date("2021-01-01")) %>%
    {if (!is.na(lastn)) slice_tail(., n=lastn) else .} %>%
    mutate(Home_Away_Short=ifelse(Home_Away=="Home","H","A"),.after="Home_Away") %>%
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
      title=glue("{shorten_team_names(team)} <b style='color:darkred'>attack</b> / <b style='color:royalblue'>defence</b> xG trend"),
      x=element_blank(),
      y=glue("Expected goals <b style='color:darkred'>for</b> / <b style='color:royalblue'>against</b>")
    ) +
    scale_x_reordered() +
    scale_y_continuous(limits=c(0,NA),expand=expansion(add=c(0,0.1))) +
    facet_grid(cols=vars(Season), space="free", scales="free_x")
  
  return(plot)
}