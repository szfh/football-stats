xg_team <- function(season,per90=TRUE,since=NA,highlight=NA,highlight_colour="darkred"){
  
  penalties <-
    data$fbref$advanced_stats_team_summary %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    filter(!is.na(PKatt)) %>%
    select(Match_Date,Home_Team,Away_Team,Home_Away,PKatt) %>%
    pivot_wider(names_from=Home_Away, values_from=PKatt, names_glue="PK_{Home_Away}")
  
  plot <-
    data$fbref$advanced_stats_team_summary %>%
    filter(Season %in% !!season) %>%
    filter((!is.na(Home_xG))|!is.na(Away_xG)) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    {if (is.na(since)) filter(., TRUE) else filter(., Match_Date>=as.Date(since))} %>%
    select(Match_Date,Home_Team,Away_Team,Home_xG,Away_xG,Team,Home_Away) %>%
    left_join(penalties) %>%
    arrange(Match_Date) %>%
    mutate(
      Home_npxG=Home_xG-(PK_Home*0.7),
      Away_npxG=Away_xG-(PK_Away*0.7)
    ) %>%
    mutate(across(c(Home_Team,Away_Team,Team),shorten_team_names)) %>%
    mutate(
      Team_npxG=ifelse(Home_Away=="Home",Home_npxG,Away_npxG),
      Opposition_npxG=ifelse(Home_Away=="Home",-Away_npxG,-Home_npxG)
    ) %>%
    select(Team,Team_npxG,Opposition_npxG) %>%
    group_by(Team) %>%
    {
      if (per90) summarise(., across(where(is.numeric),mean,na.rm=TRUE),.groups="drop")
      else summarise(., across(where(is.numeric),sum,na.rm=TRUE),.groups="drop")
    } %>%
    make_long_data(levels=c("Team_npxG","Opposition_npxG"),labels=c("Expected goals for","Expected goals against")) %>%
    mutate(focus=case_when(
      Team %in% highlight ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    ggplot(aes(x=0.05,y=n)) +
    geom_text_repel(
      aes(label=Team,
          fontface=ifelse(focus,"bold","plain"),
          colour=focus),
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
      title="Expected goals",
      x=element_blank(),
      y=element_blank()) +
    scale_x_continuous(limit=c(0,1)) +
    # scale_y_continuous(breaks=seq(-100,100,10),labels=abs(seq(-100,100,10)),expand=expansion(mult=0.0.5)) +
    scale_y_continuous(breaks=breaks_extended(8),expand=expansion(mult=c(0.05))) +
    # scale_y_continuous(breaks=breaks_extended(8),labels=abs(breaks_extended(8)),expand=expansion(mult=c(0.05))) +
    scale_colour_manual(values=c("TRUE"=highlight_colour,"FALSE"="black")) + 
    scale_fill_manual(values=palette[["epl"]]())
  
  return(plot)
}