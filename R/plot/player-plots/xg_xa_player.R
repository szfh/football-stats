xg_xa_player <- function(season){
  
  plot <-
    data$fbref$advanced_stats_player_summary %>% 
    filter(season %in% !!season) %>% 
    select(Player,Team,npxG=npxG_Expected,xA=xA_Expected) %>%
    group_by(Player,Team) %>%
    summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
    make_long_data(levels=c("npxG","xA"),labels=c("Expected Goals","Expected Assists")) %>%
    mutate(focus=case_when(
      key=="Expected Goals" & min_rank(desc(n))<=20 ~ TRUE,
      key=="Expected Assists" & min_rank(desc(n))<=20 ~ TRUE,
      TRUE ~ FALSE)) %>%
    ggplot(aes(x=0.05,y=n,alpha=focus)) +
    geom_text_repel(
      aes(label=ifelse(focus,Player,"")),
      size=2.5,
      nudge_x=0.3,
      direction="y",
      hjust=0,
      segment.size=0.4,
      box.padding=0.05
    ) +
    geom_point(aes(fill=Team),shape=23,size=3,position=position_jitterdodge(jitter.width=0,jitter.height=0,dodge.width=0.04,seed=2)) +
    theme[["solarfacet"]]() +
    facet_wrap("key",scales="free") +
    labs(
      title="Expected Goals/Expected Assists",
      x=element_blank(),
      y=element_blank()) +
    scale_x_continuous(limit=c(0,1)) +
    scale_y_continuous(breaks=breaks_extended(6)) +
    scale_fill_manual(values=palette[["epl"]]()) +
    scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.1))
  
  return(plot)
}