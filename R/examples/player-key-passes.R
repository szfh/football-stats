{
  source(here::here("R","library.R"),encoding="utf-8")
  source(here("R","themes.R"),encoding="utf-8")
  source(here("R","join","join.R"),encoding="utf-8")
  source(here("R","plot","plot-utils.R"),encoding="utf-8")
  data <- join()
  
  shots_raw <- data$fbref$match_shots
  matches_raw <- data$fbref$match_results
  summary_raw <- data$fbref$advanced_stats_player_summary
}
{
  get_assister <- function(data){
    data <-
      data %>%
      mutate(Assister=case_when(
        (SCA1_Player!=Shooting_Player & SCA1_Player!="") ~ SCA1_Player,
        (SCA2_Player!=Shooting_Player & SCA2_Player!="")~ SCA2_Player,
        TRUE ~ "None"
      ))
    
    return(data)
  }
  
  get_shot_type <- function(data){
    data <-
      data %>%
      mutate(Shot_Type=case_when(
        str_detect(Shooting_Player,"(pen)") ~ "SP",
        Shot_Notes=="Free kick" ~ "SP",
        Shot_Notes=="Penalty" ~ "SP",
        SCA1_Event=="Pass (Dead)" ~ "SP",
        SCA2_Event=="Pass (Dead)" ~ "SP",
        # SCA1_Event=="Pass (Live)" ~ "OP",
        # SCA2_Event=="Pass (Live)" ~ "OP",
        TRUE ~ "OP"
      ))
    
    return(data)
  }
}
{
  player <- "James Ward-Prowse"
  start_date <- today()-365
  
  matches <-
    summary_raw %>%
    filter(Player==player) %>%
    mutate(Date=parse_date_time(Match_Date,"mdy")) %>%
    filter(Date>=start_date) %>%
    mutate(Min_Ann=case_when(
      Min==90 ~ glue(""),
      TRUE ~ glue("**({Min})**")
    )) %>%
    select(Season,Date,Home_Team,Home_Score,Away_Team,Away_Score,Team,Home_Away,Min_Ann) %>%
    glimpse
  
  shots <-
    shots_raw %>%
    mutate(Date=as.Date(Date)) %>%
    filter(Date>=start_date) %>%
    glimpse
  
  shots_player <-
    shots %>%
    get_shot_type() %>%
    get_assister() %>%
    filter(Assister==player) %>%
    glimpse
  
  plots <- list()
  
  plots$player_key_passes <-
    bind_rows(
      shots_player %>%
        filter(Shot_Type=="OP") %>%
        count(Date,Shot_Type) %>%
        full_join(matches,.) %>%
        mutate(Shot_Type="OP"),
      shots_player %>%
        filter(Shot_Type=="SP") %>%
        count(Date) %>%
        full_join(matches,.) %>%
        mutate(Shot_Type="SP")
    ) %>% 
    arrange(Date) %>%
    mutate(n=replace_na(n,0)) %>%
    mutate(across(c(Home_Team,Away_Team),shorten_team_names)) %>%
    pivot_wider(names_from=Shot_Type,values_from=n) %>%
    mutate(Opposition=ifelse(Home_Away=="Home",Away_Team,Home_Team)) %>%
    mutate(Score=ifelse(Home_Away=="Home",glue("{Home_Score}-{Away_Score} {Min_Ann}"),glue("{Away_Score}-{Home_Score} {Min_Ann}"))) %>%
    mutate(H_A=ifelse(Home_Away=="Home","H","A")) %>%
    mutate(Match=glue("{Opposition} {H_A} {Score}")) %>%
    mutate(Match=reorder_within(Match, Date, Season)) %>%
    ggplot(aes(x=Match)) +
    geom_path(aes(y=OP,group=1),colour="darkred",size=0.75,alpha=0.6) +
    geom_path(aes(y=SP,group=1),colour="royalblue",linetype="dashed",size=0.75,alpha=1) +
    theme[["solar"]]() +
    facet_grid(cols=vars(Season), space="free", scales="free_x") +
    theme(
      plot.title=element_markdown(),
      axis.text.x=element_markdown(angle=60,hjust=1,size=rel(0.75)),
      strip.text=element_blank()
    ) +
    labs(
      title=glue("{player}<br>Shots created from <b style='color:#D71920'>open-play</b> and <b style='color:#265DAB'>set-pieces</b>"),
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_reordered() +
    scale_y_continuous(breaks=seq(0,100,1),expand=expansion(add=c(0.2)))
}
{
  plots_logo <-
    plots %>%
    add_logo(path=here("images","SB_Regular.png"),x=1,y=1,hjust=1.1,width=0.2) %>%
    add_logo(path=here("images","fbref.png"),x=0.88,y=1,hjust=1.1,width=0.29)
  
  save_plots(plots_logo,path=here("plots","test"))
}
