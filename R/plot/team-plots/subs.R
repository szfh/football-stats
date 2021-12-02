subs <- function(team,season){
  
  plot <-
    data$fbref$match_summary %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    filter(Team %in% !!team) %>%
    filter(Season %in% !!season) %>%
    filter(Event_Type=="Substitute") %>%
    select(Season,Match_Date,Home_Team,Home_Score,Away_Team,Away_Score,Team,Home_Away,Event_Half,Event_Time,Score_Progression) %>%
    mutate(Opposition=ifelse(Home_Away=="Home",Away_Team,Home_Team),.after="Team") %>%
    mutate(
      Team_Score=ifelse(Home_Away=="Home",Home_Score,Away_Score),
      Opposition_Score=ifelse(Home_Away=="Home",Away_Score,Home_Score)
    ) %>%
    separate(Score_Progression,c("Score_Progression_Home","Score_Progression_Away"),":") %>%
    type_convert() %>%
    mutate(
      Score_Progression_Home=as.numeric(Score_Progression_Home),
      Score_Progression_Away=as.numeric(Score_Progression_Away),
      Score_State=Score_Progression_Home-Score_Progression_Away,
      Score_State=ifelse(Home_Away=="Home",Score_State,-Score_State),
      Game_State=case_when(
        Score_State>0 ~ "Winning",
        Score_State<0 ~ "Losing",
        TRUE ~ "Drawing"
      )) %>%
    mutate(Home_Away_Short=ifelse(Home_Away=="Home","H","A"),.after="Home_Away") %>%
    nest_by(Season,Match_Date,Home_Team,Home_Score,Away_Team,Away_Score,Team,Opposition,Home_Away,Home_Away_Short,Team_Score,Opposition_Score) %>%
    mutate(
      Subs_Available=ifelse((Match_Date>=as_date("2020-4-1") & Match_Date<as_date("2020-9-1")),5,3),
      Subs_Used=dim(data)[1]) %>%
    ungroup() %>%
    mutate(data=pmap(list(.$data,Subs_Available,Subs_Used),possibly(get_unused_subs, otherwise=NA))) %>%
    unnest(cols=data) %>%
    mutate(Event_Period=case_when(
      Event_Half==1 ~ "First Half",
      Event_Half==2 & Event_Time==46 ~ "Half Time",
      Event_Half==2 ~ "Second Half",
      FALSE ~ "Unused",
      TRUE ~ "Other"
    ),.after="Event_Half") %>%
    mutate(Event_Period=factor(Event_Period,levels=c("First Half","Half Time","Second Half","Unused"))) %>%
    mutate(Event_Time=case_when(
      Event_Period=="First Half" ~ -2,
      Event_Period=="Half Time" ~ -1,
      Event_Period=="Unused" ~ -3,
      TRUE ~ Event_Time
    )) %>%
    mutate(Match=glue("{Team_Score}-{Opposition_Score} {Opposition} {Home_Away_Short}")) %>%
    mutate(Match=reorder_within(Match, desc(Match_Date), Season)) %>%
    ggplot(aes(x=Event_Time,y=Match,fill=Game_State)) +
    geom_beeswarm(size=2.5,shape=23,cex=1,colour="black",groupOnX=FALSE,priority="descending",alpha=0.75) +
    # geom_blank(data=data.frame(Event_Time=c(45.5,94.5),Match=c(NA,NA),Game_State=c(NA,NA),Event_Period=c("Second Half","Second Half"))) +
    theme[["solar"]]() +
    facet_grid(Season ~ Event_Period, space="free", scales="free") +
    theme(
      axis.text.x=element_text(size=5),
      axis.text.y=element_text(size=5),
      strip.text=element_blank(),
      plot.caption=element_markdown()
    ) +
    labs(
      title=glue("{team} substitutions"),
      x=element_blank(),
      y=element_blank(),
      caption="<b style='color:#60BD68'>winning</b> | <b style='color:#5DA5DA'>drawing</b> | <b style='color:#F15854'>losing</b>"
    ) +
    scale_x_continuous(breaks=c(-3,-2,-1,seq(45,130,5)),labels=c("Unused","First\nHalf","Half\nTime",seq(45,130,5)),expand=expansion(add=0.9)) +
    scale_y_reordered(expand=expansion(add=c(1.2,1.6)),position = "right") +
    scale_fill_manual(values=c("Winning"=colour[["medium"]][[3]],"Drawing"=colour[["medium"]][[1]],"Losing"=colour[["medium"]][[8]],"Unused"="Grey"))
  
  return(plot)
}