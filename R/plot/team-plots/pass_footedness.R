pass_footedness <- function(team,season){
  
  plot <-
    data$fbref$advanced_stats_player_passing_types %>%
    filter(Team %in% !!team) %>%
    filter(season %in% !!season) %>%
    select(Player,Left=Left_Body_Parts,Right=Right_Body_Parts) %>%
    group_by(Player) %>%
    summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
    mutate(All=Left+Right) %>%
    filter(All>=max(0.1*All)) %>%
    group_by(Player) %>%
    mutate(
      Most=max(Left,Right),
      Least=min(Left,Right),
      Foot1=ifelse(Left>Right,"Left","Right"),
      Foot2=ifelse(Left>=Right,"Right","Left")) %>%
    ungroup() %>%
    mutate(Ratio=Most/All) %>%
    mutate(Player=fct_reorder(Player,desc(Ratio))) %>%
    ggplot(aes(x=0,y=Player,yend=Player)) +
    geom_segment(aes(xend=Most,colour=Foot1),size=2.5,alpha=0.8) +
    geom_segment(aes(xend=-Least,colour=Foot2),size=2.5,alpha=0.8) +
    geom_label(aes(label=sprintf("%2.0f%%",100*Ratio)),size=1.6,label.padding=unit(0.16, "lines"),label.r = unit(0.08, "lines")) +
    theme[["solar"]]() +
    theme(
      plot.title=element_markdown()
    ) +
    labs(
      title=glue("<b style='color:#265DAB'>Left foot</b> / <b style='color:#CB2027'>Right foot</b> passes"),
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(breaks=seq(-2000,2000,100),labels=abs(seq(-2000,2000,100)),expand=expansion(add=c(10))) +
    scale_colour_manual(values=c("Left"=colour[["medium"]][[1]],"Right"=colour[["medium"]][[8]]))
  
  return(plot)
}