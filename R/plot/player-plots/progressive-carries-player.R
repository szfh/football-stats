progressive_carries_player <- function(season,player=NA){
  
  plot <-
    data$fbref$advanced_stats_player_possession %>%
    filter(Season %in% !!season) %>% 
    select(Player,Team,Min,Carries1=Prog_Carries,Carries2=Final_Third_Carries,Carries3=CPA_Carries) %>%
    group_by(Player,Team) %>%
    summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
    make_long_data(levels=c("Carries1","Carries2","Carries3"),labels=c("70 yards from goal","35 yards from goal","20 yards from goal")) %>%
    mutate(focus=case_when(
      min_rank(desc(n))<=20 ~ TRUE,
      Player %in% !!player ~ TRUE,
      TRUE ~ FALSE)) %>%
    mutate(highlight=ifelse(Player %in% !!player,TRUE,FALSE)) %>%
    ggplot(aes(x=0.05,y=n,alpha=focus)) +
    geom_text_repel(
      aes(
        label=ifelse(focus,Player,""),
        fontface=ifelse(highlight,"bold","plain"),
        colour=highlight),
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
      title="Progressive Carries",
      x=element_blank(),
      y=element_blank()) +
    scale_x_continuous(limit=c(0,1)) +
    scale_y_continuous(breaks=breaks_extended(6)) +
    scale_fill_manual(values=palette[["epl"]]()) +
    scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.1)) +
    scale_colour_manual(values=c("TRUE"="darkred","FALSE"="black"))
  
  return(plot)
}