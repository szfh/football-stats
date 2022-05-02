team_key_passes <- function(team,season,lastn=NA,since=NA){
  
  get_shot_type <- function(data){
    data <-
      data %>%
      mutate(Shot_Type=case_when(
        str_detect(Shooting_Player,"(pen)") ~ "SP",
        Shot_Notes=="Free kick" ~ "SP",
        Shot_Notes=="Penalty" ~ "SP",
        SCA1_Event=="Pass (Dead)" ~ "SP",
        SCA2_Event=="Pass (Dead)" ~ "SP",
        TRUE ~ "OP"
      ))
    
    return(data)
  }
  
  matches <-
    data$fbref$advanced_stats_team_summary %>%
    filter(Season %in% !!season) %>%
    filter(Team==!!team) %>%
    mutate(Date=parse_date_time(Match_Date,"mdy")) %>%
    {if (!is.na(since)) filter(., Match_Date>=as.Date(since)) else .} %>%
    {if (!is.na(lastn)) slice_tail(., n=lastn) else .} %>%
    select(Season,Date,Home_Team,Home_Score,Away_Team,Away_Score,Team,Home_Away) %>%
    mutate(across(c(Home_Team,Away_Team,Team),shorten_team_names)) %>%
    glimpse
  
  shots <-
    data$fbref$match_shots %>%
    distinct() %>%
    mutate(Date=as.Date(Date)) %>%
    get_shot_type() %>%
    select(-Home_Away) %>%
    rename(Team=Squad)
  
  plot <-
    left_join(matches,shots) %>%
    arrange(Date) %>%
    filter(Shot_Type %in% c("OP","SP")) %>%
    count(Season,Date,Home_Team,Home_Score,Away_Team,Away_Score,Team,Home_Away,Shot_Type,name="Shots") %>%
    mutate(Shots=replace_na(Shots,0)) %>%
    mutate(across(c(Home_Team,Away_Team),shorten_team_names)) %>%
    pivot_wider(names_from=Shot_Type,values_from=Shots,values_fill=0) %>%
    mutate(Opposition=ifelse(Home_Away=="Home",Away_Team,Home_Team)) %>%
    mutate(Score=ifelse(Home_Away=="Home",glue("{Home_Score}-{Away_Score}"),glue("{Away_Score}-{Home_Score}"))) %>%
    mutate(H_A=ifelse(Home_Away=="Home","H","A")) %>%
    mutate(Match=glue("{Opposition} {H_A} {Score}")) %>%
    mutate(Match=reorder_within(Match, Date, Season)) %>%
    ggplot(aes(x=Match)) +
    geom_path(aes(y=OP,group=1),colour="darkred",linetype="solid",size=0.75,alpha=0.6) +
    geom_path(aes(y=SP,group=1),colour="royalblue",linetype="longdash",size=0.75,alpha=1) +
    theme[["solar"]]() +
    facet_grid(cols=vars(Season), space="free", scales="free_x") +
    theme(
      plot.title=element_markdown(),
      axis.text.x=element_markdown(angle=60,hjust=1,size=rel(0.75)),
      axis.text.y=element_markdown(size=rel(0.75)),
      strip.text=element_blank()
    ) +
    labs(
      title=glue("{shorten_team_names(team)} shots from <b style='color:#D71920'>open-play</b> and <b style='color:#265DAB'>set-pieces</b>"),
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_reordered(expand=expansion(add=c(0.2))) +
    scale_y_continuous(breaks=seq(0,100,1),expand=expansion(add=c(0,0.2)))
  
  return(plot)
}
