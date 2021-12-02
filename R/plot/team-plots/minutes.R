team_minutes <- function(team,season){
  starting <-
    data$fbref$match_lineups %>%
    select(Match_Date=Matchday,Team,Home_Away,Player=Player_Name,Team,Starting) %>%
    filter(Starting %in% c("Pitch","Bench"))
  
  plot <-
    data$fbref$advanced_stats_player_summary %>%
    filter(Season %in% !!season) %>%
    filter(Team %in% !!team) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    select(Match_Date,Player,Min) %>%
    left_join(starting) %>%
    select(-Match_Date) %>%
    group_by(Player,Starting) %>%
    summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
    pivot_wider(names_from=Starting, values_from=Min, names_glue="Min_{Starting}") %>%
    mutate(
      Min_Pitch=replace_na(Min_Pitch,0),
      Min_Bench=replace_na(Min_Bench,0),
      Min_Total=Min_Pitch+Min_Bench) %>%
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
    scale_x_continuous(breaks=seq(0,90*38*5,90*2),expand=expansion(add=c(0,20)))
  # scale_x_continuous(breaks=function(x) seq(0, 90*38*5, by = <x>)) # axis labels as function https://stackoverflow.com/questions/15622001/how-to-display-only-integer-values-on-an-axis-using-ggplot2
  
  return(plot)
}