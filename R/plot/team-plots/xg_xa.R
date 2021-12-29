xg_xa <- function(team,season,since=NA,per90=TRUE){
  
  plot <-
    data$fbref$advanced_stats_player_summary %>%
    filter(Season %in% !!season) %>%
    filter(Team %in% !!team) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    {if (!is.na(since)) filter(., Match_Date>=as.Date(since)) else .} %>%
    select(Player,Min,npxG=npxG_Expected,xA=xA_Expected) %>%
    group_by(Player) %>%
    summarise(., across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
    filter(Min>=0.1*max(Min)) %>%
    {if(per90) mutate(., npxG=90*npxG/Min, xA=90*xA/Min) else mutate(., npxG=npxG, xA=xA)} %>%
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
    scale_x_continuous(limits=c(0,NA),breaks=breaks_extended(8),expand=expansion(mult=c(0,0.05))) +
    scale_y_continuous(limits=c(0,NA),breaks=breaks_extended(8),expand=expansion(mult=c(0,0.05))) +
    scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["main"]],"FALSE"=colour[["sfc"]][["grey"]]))
  
  return(plot)
}