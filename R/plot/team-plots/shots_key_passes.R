shots_key_passes <- function(team,season){
  
  plot <-
    inner_join(
      data$fbref$advanced_stats_player_summary %>%
        select(season,Team,Player,Match_Date,Sh),
      data$fbref$advanced_stats_player_passing %>%
        select(season,Team,Player,Match_Date,KP)
    ) %>%
    filter(Team %in% !!team) %>%
    filter(season %in% !!season) %>%
    select(Player,Sh,KP) %>%
    group_by(Player) %>%
    summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
    make_long_data(levels=c("Sh","KP"),labels=c("Shots","Passes leading to shots")) %>%
    mutate(focus=case_when(
      n==0 ~ FALSE,
      min_rank(desc(n))<=8 ~ TRUE,
      TRUE ~ FALSE)) %>%
    ggplot(aes(x=0,y=n)) +
    geom_text_repel(
      aes(label=ifelse(focus,Player,"")),
      size=rel(3),
      nudge_x=0.3,
      direction="y",
      hjust=0,
      segment.size=0.4,
      box.padding=0.05
    ) +
    geom_point(aes(colour=focus,fill=focus),shape=23,size=2) +
    theme[["solarfacet"]]() +
    facet_wrap("key",scales="free") +
    labs(
      title="Shots and Shot Creating Passes",
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(limit=c(0,1)) +
    scale_y_continuous(breaks=breaks_extended(5)) +
    scale_colour_manual(values=c("TRUE"=colour[["sfc"]][["black"]],"FALSE"=colour[["sfc"]][["grey"]])) +
    scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["light"]],"FALSE"=colour[["sfc"]][["grey"]]))
  
  return(plot)
}
