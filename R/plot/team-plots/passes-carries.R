passes_carries <- function(team,season,since=NA,per90=TRUE){
  
  plot <-
    inner_join(
      data$fbref$advanced_stats_player_possession %>%
        select(season,Match_Date,Team,Player,Min,Carries=Final_Third_Carries),
      data$fbref$advanced_stats_player_passing %>%
        select(season,Match_Date,Team,Player,Min,Passes=Final_Third)
    ) %>%
    filter(season %in% !!season) %>%
    filter(Team %in% !!team) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"ymd")) %>%
    {if (!is.na(since)) filter(., Match_Date>=as.Date(since)) else .} %>%
    select(Player,Min,Carries,Passes) %>%
    group_by(Player) %>%
    summarise(., across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
    filter(Min>=0.1*max(Min)) %>%
    {if(per90) mutate(., Carries=90*Carries/Min, Passes=90*Passes/Min) else mutate(., Passes=Passes, Carries=Carries)} %>%
    mutate(Focus=case_when(
      Carries==0 & Passes==0 ~ FALSE,
      min_rank(desc(Carries))<=8 ~ TRUE,
      min_rank(desc(Passes))<=8 ~ TRUE,
      TRUE ~ FALSE)) %>%
    ggplot(aes(x=Passes,y=Carries)) +
    geom_point(aes(fill=Focus),shape=23,size=2.5,alpha=0.8,colour=colour[["sfc"]][["black"]]) +
    geom_text_repel(aes(label=ifelse(Focus,Player,"")),size=2.5) +
    theme[["solar"]]() +
    labs(
      title=glue("Carries and Passes to the attacking third"),
      x="Passes",
      y="Carries"
    ) +
    scale_x_continuous(limits=c(0,NA),breaks=breaks_extended(8),expand=expansion(mult=c(0,0.05))) +
    scale_y_continuous(limits=c(0,NA),breaks=breaks_extended(8),expand=expansion(mult=c(0,0.05))) +
    scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["main"]],"FALSE"=colour[["sfc"]][["grey"]]))
  
  return(plot)
}
