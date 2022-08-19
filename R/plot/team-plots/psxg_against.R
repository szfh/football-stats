psxg_against <- function(team,season,since=NA){
  
  plot <-
    inner_join(
      data$fbref$advanced_stats_team_keeper %>%
        select(season,Team,Match_Date,GA=GA_Shot_Stopping,psxG=PSxG_Shot_Stopping,SoTA=SoTA_Shot_Stopping),
      data$fbref$advanced_stats_team_misc %>%
        select(season,Team,Match_Date,OG)
    ) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"ymd")) %>%
    {if (!is.na(since)) filter(., Match_Date>=as.Date(since)) else .} %>%
    filter(season %in% !!season) %>%
    filter(!is.na(psxG)) %>%
    bind_rows(
      tibble(
        Team=data$fbref$advanced_stats_team_keeper %>% filter(season %in% !!season) %>% pull(Team) %>% unique(),
        Match_Date=as.Date("1970-01-01"),
        SoTA=0,
        GA=0,
        psxG=0,
        OG=0
      )
    ) %>%
    mutate(psxGD=psxG-(GA-OG)) %>%
    arrange(Match_Date) %>%
    group_by(Team) %>%
    mutate(
      cumulative_psxGD=cumsum(psxGD),
      cumulative_SoTA=cumsum(SoTA)
    ) %>%
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
      # title=glue("GK expected saves - <b style='color:darkred'>{team}</b>"),
      title=glue("Post-shot expected goals"),
      x="Shots on target faced",
      y="GK over/under performance"
    ) +
    scale_x_continuous(breaks=breaks_extended(8),expand=expansion(add=c(0,1))) +
    scale_y_continuous(breaks=breaks_extended(8),expand=expansion(add=c(0.2,0.2))) +
    scale_colour_manual(values=c("TRUE"="darkred","FALSE"="darkgray")) +
    scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.5))
  
  return(plot)
}
