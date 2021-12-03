xg_team_scatter <- function(season){
  
  penalties <-
    data$fbref$advanced_stats_team_summary %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    filter(!is.na(PKatt)) %>%
    select(Match_Date,Home_Team,Away_Team,Home_Away,PKatt) %>%
    pivot_wider(names_from=Home_Away, values_from=PKatt, names_glue="PK_{Home_Away}")
  
  plot <-
    data$fbref$advanced_stats_team_summary %>%
    filter(Season %in% !!season) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    select(Match_Date,Home_Team,Away_Team,Home_xG,Away_xG,Team,Home_Away) %>%
    left_join(penalties) %>%
    mutate(
      Home_npxG=Home_xG-(PK_Home*0.7),
      Away_npxG=Away_xG-(PK_Away*0.7)
    ) %>%
    mutate(
      Team_npxG=ifelse(Home_Away=="Home",Home_npxG,Away_npxG),
      Opposition_npxG=ifelse(Home_Away=="Home",Away_npxG,Home_npxG)
    ) %>%
    select(Team,Team_npxG,Opposition_npxG) %>%
    group_by(Team) %>%
    summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
    ggplot(aes(x=Team_npxG,y=Opposition_npxG)) +
    geom_text_repel(aes(label=Team),size=2) +
    geom_point(aes(fill=Team),shape=23,size=2.5) +
    theme[["solar"]]() +
    labs(
      title="Expected goals",
      x="xG for",
      y="xG against"
    ) +
    scale_x_continuous(breaks=breaks_extended(6),expand=expansion(add=c(2))) +
    scale_y_reverse(breaks=breaks_extended(6),expand=expansion(add=c(2))) +
    scale_fill_manual(values=palette[["epl"]]())
  
  return(plot)
}