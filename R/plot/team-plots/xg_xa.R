xg_xa <- function(team,season){
  
  plot <-
    data$fbref$advanced_stats_player_summary %>%
    filter(Season %in% !!season) %>%
    filter(Team %in% !!team) %>%
    select(Player,Min,npxG=npxG_Expected,xA=xA_Expected) %>%
    group_by(Player) %>%
    summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
    mutate(Focus=case_when(
      npxG==0 & xA==0 ~ FALSE,
      min_rank(desc(npxG))<=8 ~ TRUE,
      min_rank(desc(xA))<=8 ~ TRUE,
      TRUE ~ FALSE)) %>%
    ggplot(aes(x=npxG,y=xA)) +
    geom_point(aes(fill=Focus),shape=23,size=2.5,alpha=0.8,colour=colour[["sfc"]][["black"]]) +
    geom_text_repel(aes(label=ifelse(Focus,Player,"")),size=rel(2.5)) +
    theme[["solar"]]() +
    labs(
      title=glue("{team} xG/xA"),
      x="Expected goals (penalties excluded)",
      y="Expected assists"
    ) +
    scale_x_continuous(limits=c(0,NA),breaks=breaks_extended(6),expand=expansion(add=c(0,0.2))) +
    scale_y_continuous(limits=c(0,NA),breaks=breaks_extended(6),expand=expansion(add=c(0,0.2))) +
    scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["main"]],"FALSE"=colour[["sfc"]][["grey"]]))
  
  return(plot)
}