psxg_for <- function(season){
  
  plot <-
    data$fbref$season_stat_keeper_adv %>%
    filter(season %in% !!season) %>%
    filter(team_or_player=="team") %>%
    filter(Team_or_Opponent=="team") %>%
    select(Squad,GA=GA_Goals,OG=OG_Goals,PSxG=PSxG_Expected,PSxGSOT=PSxG_per_SoT_Expected,PSxGD=PSxGPlus_Minus_Expected) %>%
    mutate(Plus_Minus=ifelse(PSxGD>=0,TRUE,FALSE)) %>%
    mutate(Squad=fct_reorder(Squad,PSxGD)) %>%
    ggplot(aes(x=0,xend=PSxGD,y=Squad,yend=Squad,colour=Plus_Minus)) +
    geom_segment(size=3.5,alpha=0.8) +
    theme[["solar"]]() +
    theme(axis.text.y=element_markdown()) +
    labs(
      title="Post-shot expected goals",
      x=element_blank(),
      y=element_blank()
    ) +
    annotate("text",label="Goalkeepers\noverperforming\nshot-stopping",fontface="bold",hjust="right",size=3,x=-0.6,y=17) +
    annotate("text",label="Goalkeepers\nunderperforming\nshot-stopping",fontface="bold",hjust="left",size=3,x=0.6,y=4) +
    scale_x_continuous(breaks=breaks_extended(15),expand=expansion(mult=c(0.05,0.05))) +
    scale_colour_manual(values=c("TRUE"=colour[["medium"]][[3]],"FALSE"=colour[["medium"]][[8]]))
  
  return(plot)
}