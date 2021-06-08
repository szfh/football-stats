source(here("R","plot","plot-utils.R"),encoding="utf-8")
source(here("R","themes.R"),encoding="utf-8")

plot_team_wfr <- function(data,team="Southampton",season="2020-2021"){
  
  force(data)
  plots <- list()
  
  # team <- expand_team(team)
  season <- expand_seasons(season)
  
  penalties <-
    data$fbref$advanced_stats_team_summary %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    filter(!is.na(PKatt)) %>%
    filter((Home_Team %in% !!team)|(Away_Team %in% !!team)) %>%
    select(Match_Date,Home_Away,PKatt) %>%
    pivot_wider(names_from=Home_Away, values_from=PKatt, names_glue="PK_{Home_Away}")
  
  plots$xgtrendmva <-
    data$fbref$advanced_stats_team_summary %>%
    filter(Team %in% !!team) %>%
    filter((!is.na(Home_xG))|!is.na(Away_xG)) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    select(Season,Match_Date,Team,Home_Team,Home_Score,Home_xG,Away_Team,Away_Score,Away_xG,Home_Away) %>%
    left_join(penalties) %>%
    arrange(Match_Date) %>%
    mutate(
      Home_npxG=Home_xG-(PK_Home*0.7),
      Away_npxG=Away_xG-(PK_Away*0.7)
    ) %>%
    mutate(Opposition=ifelse(Home_Away=="Home",Away_Team,Home_Team),.after="Team") %>%
    mutate(
      Team_Score=ifelse(Home_Away=="Home",Home_Score,Away_Score),
      Team_npxG=ifelse(Home_Away=="Home",Home_npxG,Away_npxG),
      Opposition_Score=ifelse(Home_Away=="Home",Away_Score,Home_Score),
      Opposition_npxG=ifelse(Home_Away=="Home",Away_npxG,Home_npxG)
    ) %>%
    mutate(
      Team_npxG_mva=get_mva(Team_npxG),
      Opposition_npxG_mva=get_mva(Opposition_npxG)) %>%
    filter(Season %in% !!season) %>%
    mutate(Season=case_when(
      Match_Date>=as.Date("2019-08-01") & Match_Date<as.Date("2020-04-01") ~ "2019-20 part 1",
      Match_Date>=as.Date("2020-04-01") & Match_Date<as.Date("2020-08-01") ~ "2019-20 part 2",
      TRUE ~ Season)) %>%
    mutate(Home_Away_Short=ifelse(Home_Away=="Home","H","A"),.after="Home_Away") %>%
    mutate(Match=glue::glue("{Opposition} {Home_Away_Short} {Team_Score}-{Opposition_Score}")) %>%
    mutate(Match=reorder_within(Match, Match_Date, Season)) %>%
    ggplot(aes(x=Match)) +
    geom_point(aes(y=Team_npxG),size=1,colour="darkred",fill="darkred",alpha=0.5,shape=23) +
    geom_line(aes(y=Team_npxG_mva,group=Season),colour="darkred",linetype="longdash",size=0.7) +
    geom_point(aes(y=Opposition_npxG),size=1,colour="royalblue",fill="royalblue",alpha=0.5,shape=23) +
    geom_line(aes(y=Opposition_npxG_mva,group=Season),colour="royalblue",linetype="longdash",size=0.7) +
    theme[["solar"]]() +
    theme(
      axis.text.x=element_text(size=6,angle=60,hjust=1),
      axis.title.y=element_markdown(),
      axis.text.y=element_text(),
      plot.title=element_markdown(),
      plot.caption=element_text(),
      strip.text=element_blank()
    ) +
    labs(
      title=glue("{team} <b style='color:darkred'>attack</b> / <b style='color:royalblue'>defence</b> xG trend"),
      x=element_blank(),
      y=glue("Expected goals <b style='color:darkred'>for</b> / <b style='color:royalblue'>against</b>")
    ) +
    scale_x_reordered() +
    scale_y_continuous(limits=c(0,NA),expand=expansion(add=c(0,0.1))) +
    facet_grid(cols=vars(Season), space="free", scales="free_x")
  
  plots$xgsegment <-
    data$fbref$advanced_stats_team_summary %>%
    filter(Team %in% !!team) %>%
    filter((!is.na(Home_xG))|!is.na(Away_xG)) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    select(Season,Match_Date,Team,Home_Team,Home_Score,Home_xG,Away_Team,Away_Score,Away_xG,Home_Away) %>%
    left_join(penalties) %>%
    arrange(Match_Date) %>%
    mutate(
      Home_npxG=Home_xG-(PK_Home*0.7),
      Away_npxG=Away_xG-(PK_Away*0.7)
    ) %>%
    mutate(Opposition=ifelse(Home_Away=="Home",Away_Team,Home_Team),.after="Team") %>%
    mutate(
      Team_Score=ifelse(Home_Away=="Home",Home_Score,Away_Score),
      Team_npxG=ifelse(Home_Away=="Home",Home_npxG,Away_npxG),
      Opposition_Score=ifelse(Home_Away=="Home",Away_Score,Home_Score),
      Opposition_npxG=ifelse(Home_Away=="Home",Away_npxG,Home_npxG)
    ) %>%
    filter(Season %in% !!season) %>%
    mutate(Season=case_when(
      Match_Date>=as.Date("2019-08-01") & Match_Date<as.Date("2020-04-01") ~ "2019-20 part 1",
      Match_Date>=as.Date("2020-04-01") & Match_Date<as.Date("2020-08-01") ~ "2019-20 part 2",
      TRUE ~ Season)) %>%
    mutate(Home_Away_Short=ifelse(Home_Away=="Home","H","A")) %>%
    mutate(Match=glue::glue("{Opposition} {Home_Away_Short} {Team_Score}-{Opposition_Score}")) %>%
    mutate(Match=reorder_within(Match, desc(Match_Date), Season)) %>%
    ggplot(aes(y=Match,yend=Match)) +
    geom_segment(aes(x=0,xend=Team_npxG),colour=colour[["sfc"]][["light"]],size=2) +
    geom_segment(aes(x=0,xend=-Opposition_npxG),colour=colour[["medium"]][[1]],size=2) +
    theme[["solar"]]() +
    theme(
      plot.title=element_markdown(size=8),
      axis.text.x=element_text(),
      axis.text.y=element_text(size=6),
      strip.text=element_blank()
    ) +
    labs(
      title=glue("<b style='color:#265DAB'>Opposition xG</b> | <b style='color:#D71920'>{team} xG</b>"),
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(breaks=seq(-10,10,1),labels=abs(seq(-10,10,1)),expand=expansion(add=c(0.1,1))) +
    scale_y_reordered() +
    facet_grid(rows=vars(Season), space="free", scales="free_y")
  
  starting <-
    data$fbref$match_lineups %>%
    select(Match_Date=Matchday,Team,Home_Away,Player=Player_Name,Team,Starting) %>%
    filter(Starting %in% c("Pitch","Bench"))
  
  plots$minutes <-
    data$fbref$advanced_stats_player_summary %>%
    filter(Season %in% !!season) %>%
    filter(Team %in% !!team) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
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
    mutate(Player=fct_reorder(Player,Min_Total)) %>%
    ggplot(aes(y=Player)) +
    geom_segment(aes(y=Player,yend=Player,x=0,xend=Min_Pitch),colour=colour[["sfc"]][["main"]],size=2.5,alpha=0.8) +
    geom_segment(aes(y=Player,yend=Player,x=Min_Pitch,xend=Min_Total),colour=colour[["sfc"]][["light"]],size=2.5,alpha=0.8) +
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
    scale_x_continuous(breaks=seq(0,90*38*3,180),expand=expansion(add=c(0,20)))
  
  plots$xgxa <-
    data$fbref$advanced_stats_player_summary %>%
    filter(Season %in% !!season) %>%
    filter(Team %in% !!team) %>%
    select(Player,Min,npxG=npxG_Expected,xA=xA_Expected) %>%
    group_by(Player) %>%
    summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
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
    scale_x_continuous(limits=c(0,NA),breaks=seq(0,30,1),expand=expansion(add=c(0,0.2))) +
    scale_y_continuous(limits=c(0,NA),breaks=seq(0,30,1),expand=expansion(add=c(0,0.2))) +
    scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["main"]],"FALSE"=colour[["sfc"]][["grey"]]))
  
  plots$xgxa90 <-
    data$fbref$advanced_stats_player_summary %>%
    filter(Season %in% !!season) %>%
    filter(Team %in% !!team) %>%
    select(Player,Min,npxG=npxG_Expected,xA=xA_Expected) %>%
    group_by(Player) %>%
    summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
    filter(Min>=900) %>%
    mutate(npxG=90*npxG/Min,
           xA=90*xA/Min) %>%
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
      title=glue("{team} xG/xA per 90"),
      x="Expected goals per 90 mins (penalties excluded)",
      y="Expected assists per 90 mins"
    ) +
    scale_x_continuous(limits=c(0,NA),breaks=seq(0,5,0.1),expand=expansion(add=c(0,0.02))) +
    scale_y_continuous(limits=c(0,NA),breaks=seq(0,5,0.1),expand=expansion(add=c(0,0.02))) +
    scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["main"]],"FALSE"=colour[["sfc"]][["grey"]]))
  
  plots$shots_keypasses <-
    data$fbref$advanced_stats_player_summary %>%
    left_join(data$fbref$advanced_stats_player_passing) %>%
    filter(Team %in% !!team) %>%
    filter(Season %in% !!season) %>%
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
    scale_y_continuous() +
    scale_colour_manual(values=c("TRUE"=colour[["sfc"]][["black"]],"FALSE"=colour[["sfc"]][["grey"]])) +
    scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["light"]],"FALSE"=colour[["sfc"]][["grey"]]))
  
  plots$psxg_against <-
    data$fbref$advanced_stats_team_keeper %>%
    left_join(data$fbref$advanced_stats_team_misc) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    filter(Season %in% !!season) %>%
    select(Team,Match_Date,GA=GA_Shot_Stopping,psxG=PSxG_Shot_Stopping,OG) %>%
    mutate(psxGD=psxG-(GA-OG)) %>%
    arrange(Match_Date) %>%
    group_by(Team) %>%
    mutate(cumulative_psxGD=cumsum(psxGD)) %>%
    ungroup() %>%
    mutate(focus=ifelse(Team %in% !!team,TRUE,FALSE)) %>%
    ggplot(aes(x=Match_Date,y=cumulative_psxGD)) +
    geom_path(aes(group=Team,alpha=focus,colour=focus),size=0.75) +
    theme[["solar"]]() +
    theme(
      axis.text.x=element_markdown(),
      axis.title.y=element_markdown(),
      axis.text.y=element_text(),
      plot.title=element_markdown(),
      plot.caption=element_text(),
      strip.text=element_blank()
    ) +
    labs(
      title=glue("GK expected saves - <b style='color:darkred'>{team}</b>"),
      x=element_blank(),
      y="Post-shot xG performance"
    ) +
    scale_x_datetime(expand=expansion(add=100)) +
    scale_y_continuous(expand=expansion(add=0.2)) +
    scale_colour_manual(values=c("TRUE"="darkred","FALSE"="darkgray")) +
    scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.5))
  
  plots$passfootedness <-
    data$fbref$advanced_stats_player_passing_types %>%
    filter(Team %in% !!team) %>%
    filter(Season %in% !!season) %>%
    select(Player,Left=Left_Body_Parts,Right=Right_Body_Parts) %>%
    group_by(Player) %>%
    summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
    mutate(All=Left+Right) %>%
    filter(All>100) %>%
    group_by(Player) %>%
    mutate(
      Most=max(Left,Right),
      Least=min(Left,Right),
      Foot1=ifelse(Left>Right,"Left","Right"),
      Foot2=ifelse(Left>=Right,"Right","Left")) %>%
    ungroup() %>%
    mutate(Ratio=Most/All) %>%
    mutate(Player=fct_reorder(Player,desc(Ratio))) %>%
    ggplot(aes(x=0,y=Player,yend=Player)) +
    geom_segment(aes(xend=Most,colour=Foot1),size=2.5,alpha=0.8) +
    geom_segment(aes(xend=-Least,colour=Foot2),size=2.5,alpha=0.8) +
    geom_label(aes(label=sprintf("%2.0f%%",100*Ratio)),size=1.6,label.padding=unit(0.16, "lines"),label.r = unit(0.08, "lines")) +
    theme[["solar"]]() +
    theme(
      plot.title=element_markdown()
    ) +
    labs(
      title=glue("<b style='color:#265DAB'>Left foot</b> / <b style='color:#CB2027'>Right foot</b> passes"),
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(breaks=seq(-2000,2000,200),labels=abs(seq(-2000,2000,200)),expand=expansion(add=c(10))) +
    scale_colour_manual(values=c("Left"=colour[["medium"]][[1]],"Right"=colour[["medium"]][[8]]))
  
  plots$subs <-
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
      caption="Gamestate - <b style='color:#60BD68'>winning</b> | <b style='color:#5DA5DA'>drawing</b> | <b style='color:#F15854'>losing</b>"
    ) +
    scale_x_continuous(breaks=c(-3,-2,-1,seq(45,130,5)),labels=c("Unused","First\nHalf","Half\nTime",seq(45,130,5)),expand=expansion(add=0.9)) +
    scale_y_reordered(expand=expansion(add=c(1.2,1.6)),position = "right") +
    scale_fill_manual(values=c("Winning"=colour[["medium"]][[3]],"Drawing"=colour[["medium"]][[1]],"Losing"=colour[["medium"]][[8]],"Unused"="Grey"))
  
  plots_logo <-
    plots %>%
    add_logo(path=here("images","SB_Regular.png"),x=1,y=1,hjust=1.1,width=0.2) %>%
    add_logo(path=here("images","fbref.png"),x=0.88,y=1,hjust=1.1,width=0.29)
  
  save_plots(plots_logo,path=here("plots","team_wfr"))
}
