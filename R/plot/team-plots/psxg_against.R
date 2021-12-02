psxg_against <- function(team,season){
  
  plot <-
    data$fbref$advanced_stats_team_keeper %>%
    left_join(data$fbref$advanced_stats_team_misc) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    filter(Season %in% !!season) %>%
    select(Team,Match_Date,GA=GA_Shot_Stopping,psxG=PSxG_Shot_Stopping,SoTA=SoTA_Shot_Stopping,OG) %>%
    mutate(psxGD=psxG-(GA-OG)) %>%
    arrange(Match_Date) %>%
    group_by(Team) %>%
    mutate(cumulative_psxGD=cumsum(psxGD)) %>%
    mutate(cumulative_SoTA=cumsum(SoTA)) %>%
    ungroup() %>%
    mutate(focus=ifelse(Team %in% !!team,TRUE,FALSE)) %>%
    ggplot(aes(x=cumulative_SoTA,y=cumulative_psxGD)) +
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
      title=glue("GK expected saves - <b style='color:darkred'>{team}</b>"),
      x="Shots on target faced",
      y="Post-shot xG performance"
    ) +
    scale_x_continuous(expand=expansion(add=1)) +
    scale_y_continuous(expand=expansion(add=0.2)) +
    scale_colour_manual(values=c("TRUE"="darkred","FALSE"="darkgray")) +
    scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.5))
  
  return(plot)
}