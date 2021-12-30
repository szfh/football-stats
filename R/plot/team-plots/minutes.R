team_minutes <- function(team,season,since=NA,scale_factor=1){
  positions <-
    data$fbref$match_lineups %>%
    {if (!is.na(since)) filter(., Matchday>=as.Date(since)) else filter(., Matchday>=as.Date(today()-365))} %>%
    select(Player=Player_Name,Pos,Min) %>%
    mutate(Pos=str_sub(Pos,1,2)) %>%
    filter(!is.na(Pos)) %>%
    mutate(Pos=case_when(
      Pos=="LB" ~ "FB",
      Pos=="RB" ~ "FB",
      Pos=="WB" ~ "FB",
      Pos=="DM" ~ "CM",
      Pos=="LM" ~ "AM",
      Pos=="RM" ~ "AM",
      Pos=="LW" ~ "AM",
      Pos=="RW" ~ "AM",
      Pos=="FW" ~ "ST",
      TRUE ~ Pos
    )) %>%
    mutate(Pos=factor(Pos,levels=c("GK","CB","FB","CM","AM","ST"))) %>%
    # mutate(Pos=factor(Pos,levels=c("GK","CB","LB","RB","DM","CM","LM","RM","FW")))
    group_by(Player,Pos) %>%
    summarise(Min=sum(Min,na.rm=TRUE),.groups="drop") %>%
    group_by(Player) %>%
    slice_max(Min, with_ties=FALSE) %>%
    ungroup() %>%
    select(Player,Pos)
  
  starting <-
    data$fbref$match_lineups %>%
    select(Match_Date=Matchday,Team,Home_Away,Player=Player_Name,Team,Starting) %>%
    filter(Starting %in% c("Pitch","Bench"))
  
  plot <-
    data$fbref$advanced_stats_player_summary %>%
    filter(Season %in% !!season) %>%
    filter(Team %in% !!team) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    {if (!is.na(since)) filter(., Match_Date>=as.Date(since)) else .} %>%
    select(Match_Date,Player,Min) %>%
    left_join(starting) %>%
    select(-Match_Date) %>%
    group_by(Player,Starting) %>%
    summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
    pivot_wider(names_from=Starting, values_from=Min, names_glue="Min_{Starting}") %>%
    mutate(
      Min_Pitch=replace_na(Min_Pitch,0),
      Min_Bench=replace_na(Min_Bench,0),
      Min_Total=Min_Pitch+Min_Bench) %>%
    left_join(positions) %>%
    mutate(Player=fct_reorder(Player,Min_Total)) %>%
    ggplot(aes(y=Player)) +
    geom_segment(aes(y=Player,yend=Player,x=0,xend=Min_Pitch),colour=colour[["sfc"]][["main"]],size=2.5,alpha=0.8) +
    geom_segment(aes(y=Player,yend=Player,x=Min_Pitch,xend=Min_Total),colour=colour[["sfc"]][["light"]],size=2.5,alpha=0.8) +
    facet_grid(Pos ~ ., scales="free", space="free") +
    theme[["solar"]]() +
    theme(
      plot.title=element_markdown(),
      axis.line=element_blank(),
      axis.text=element_text(size=7),
      strip.text.y=element_text(angle=0)
    ) +
    labs(
      title=glue("League minutes<br />(<b style='color:#D71920'>from start</b> | <b style='color:#ED5C5C'>from bench</b>)"),
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(breaks=seq(0,90*38*5,90*scale_factor),expand=expansion(add=c(0,10*scale_factor)))
  # scale_x_continuous(breaks=function(x) seq(0, 90*38*5, by = <x>)) # axis labels as function https://stackoverflow.com/questions/15622001/how-to-display-only-integer-values-on-an-axis-using-ggplot2
  
  return(plot)
}