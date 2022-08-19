xg_segment <- function(team,season,lastn=NA,since=NA){
  penalties <-
    data$fbref$advanced_stats_team_summary %>%
    mutate(Match_Date=parse_date_time(Match_Date,"ymd")) %>%
    filter(!is.na(PKatt)) %>%
    filter((Home_Team %in% !!team)|(Away_Team %in% !!team)) %>%
    select(Match_Date,Home_Away,PKatt) %>%
    pivot_wider(names_from=Home_Away, values_from=PKatt, names_glue="PK_{Home_Away}")
  
  plot <-
    data$fbref$advanced_stats_team_summary %>%
    filter(season %in% !!season) %>%
    filter(Team %in% !!team) %>%
    filter((!is.na(Home_xG))|!is.na(Away_xG)) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"ymd")) %>%
    select(season,Match_Date,Team,Home_Team,Home_Score,Home_xG,Away_Team,Away_Score,Away_xG,Home_Away) %>%
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
    # mutate(Season=case_when(
    #   Match_Date>=as.Date("2019-08-01") & Match_Date<as.Date("2020-04-01") ~ "2019-20 part 1",
    #   Match_Date>=as.Date("2020-04-01") & Match_Date<as.Date("2020-08-01") ~ "2019-20 part 2",
    #   TRUE ~ Season)) %>%
    {if (!is.na(since)) filter(., Match_Date>=as.Date(since)) else .} %>%
    {if (!is.na(lastn)) slice_tail(., n=lastn) else .} %>%
    mutate(Home_Away_Short=ifelse(Home_Away=="Home","H","A")) %>%
    mutate(Match=glue::glue("{Opposition} {Home_Away_Short} {Team_Score}-{Opposition_Score}")) %>%
    mutate(Match=reorder_within(Match, desc(Match_Date), season)) %>%
    ggplot(aes(y=Match,yend=Match)) +
    geom_segment(aes(x=0,xend=Team_npxG),colour=colour[["sfc"]][["light"]],size=2) +
    geom_segment(aes(x=0,xend=-Opposition_npxG),colour=colour[["medium"]][[1]],size=2) +
    theme[["solar"]]() +
    theme(
      plot.title=element_markdown(size=8),
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
    facet_grid(rows=vars(season), space="free", scales="free_y")
  
  return(plot)
}
