source(here("R","plot","plot-utils.R"))

plot_team <- function(data,squad="Southampton",season="2020-21"){
  squadvs <- paste0("vs ",squad)
  
  plots <- list()
  
  plots$minutes <-
    data$fbref$players %>%
    filter(season %in% !!season) %>%
    filter(squad %in% !!squad) %>%
    select(player,pos=pos1,min=playing_time_min,starts=starts_starts,subs=subs_subs,min_start=starts_mnstart,min_sub=subs_mnsub) %>%
    mutate(min_start=replace_na(min_start,0)) %>%
    mutate(min_sub=replace_na(min_sub,0)) %>%
    group_by(player,pos) %>%
    summarise(across(min:subs,sum),across(min_start:min_sub,mean)) %>%
    # glimpse
    filter_na("min") %>%
    ungroup() %>%
    mutate(
      min_start=min_start*starts,
      min_sub=min_sub*subs
    ) %>%
    mutate(
      min_start = ifelse(is.na(min_start),0,min_start),
      min_sub = ifelse(is.na(min_sub),0,min_sub)
    ) %>%
    mutate(
      pos=case_when(
        player %in% c("Kevin Danso","Jannik Vestergaard","Jan Bednarek","Jack Stephens","Maya Yoshida","Mohammed Salisu","Allan Tchaptchet") ~ "CB",
        player %in% c("Ryan Bertrand","Cédric Soares","Yan Valery","Kyle Walker-Peters","Jake Vokins","Kayne Ramsey") ~ "FB",
        player %in% c("James Ward-Prowse","Pierre Højbjerg","Oriol Romeu","William Smallbone","Ibrahima Diallo","Alexandre Jankewitz") ~ "DM",
        player %in% c("Nathan Redmond","Stuart Armstrong","Sofiane Boufal","Moussa Djenepo","Nathan Tella","Takumi Minamino","Caleb Watts") ~ "AM",
        player %in% c("Theo Walcott") ~ "FW",
        TRUE ~ pos)
    ) %>%
    mutate(pos=factor(pos,levels=c("GK","DF","CB","FB","MF","DM","AM","FW"))) %>%
    mutate(player=fct_reorder(player,min)) %>%
    ggplot(aes(x=min,y=player)) +
    geom_segment(aes(y=player,yend=player,x=0,xend=min_start),colour=colour[["sfc"]][["main"]],size=2.5,alpha=0.8) +
    geom_segment(aes(y=player,yend=player,x=min_start,xend=min_start+min_sub),colour=colour[["sfc"]][["light"]],size=2.5,alpha=0.8) +
    theme[["solar"]]() +
    theme(
      plot.title=element_markdown(),
      axis.line=element_blank(),
      axis.text=element_text(size=7),
      strip.text.y=element_text(angle=0)
    ) +
    facet_grid(pos ~ ., space="free", scales="free_y") +
    labs(
      title=glue("League minutes<br />(<b style='color:#D71920'>from start</b> | <b style='color:#ED5C5C'>from bench</b>)"),
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(breaks=seq(0,90*38*3,180),expand=expansion(add=c(0,20)))
  
  plots$xgxa <-
    data$fbref$players %>%
    filter(season %in% !!season) %>%
    filter(squad %in% !!squad) %>%
    select(player,npxg=expected_npxg,xa=expected_xa) %>%
    filter_na(c("npxg","xa")) %>%
    # mutate(focus=ifelse(npxg>=0.1|xa>=0.1,TRUE,FALSE)) %>%
    mutate(focus=case_when(
      npxg==0 & xa==0 ~ FALSE,
      min_rank(desc(npxg))<=8 ~ TRUE,
      min_rank(desc(xa))<=8 ~ TRUE,
      TRUE ~ FALSE)) %>%
    ggplot(aes(x=npxg,y=xa)) +
    geom_point(aes(fill=focus),shape=23,size=2.5,alpha=0.8,colour=colour[["sfc"]][["black"]]) +
    geom_text_repel(aes(label=ifelse(focus,player,"")),size=rel(2.5)) +
    theme[["solar"]]() +
    labs(
      title=glue("{squad} xG/xA"),
      x="Expected goals",
      y="Expected assists"
    ) +
    scale_x_continuous(limits=c(0,NA),breaks=seq(0,30,1),expand=expansion(add=c(0,0.2))) +
    scale_y_continuous(limits=c(0,NA),breaks=seq(0,30,1),expand=expansion(add=c(0,0.2))) +
    scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["main"]],"FALSE"=colour[["sfc"]][["grey"]]))
  
  plots$shotskp <-
    data$fbref$players %>%
    filter(season %in% !!season) %>%
    filter(squad %in% !!squad) %>%
    select(player,sh=standard_sh,kp) %>%
    make_long_data(levels=c("sh","kp"),labels=c("Shot","Pass leading to shot")) %>%
    # mutate(focus=case_when(percent_rank(n)>0.4 ~ TRUE,
    #                        TRUE ~ FALSE)) %>%
    mutate(focus=case_when(
      n==0 ~ FALSE,
      min_rank(desc(n))<=15 ~ TRUE,
      TRUE ~ FALSE)) %>%
    ggplot(aes(x=0,y=n)) +
    geom_text_repel(
      aes(label=ifelse(focus,player,"")),
      size=rel(3),
      nudge_x=0.3,
      direction="y",
      hjust=0,
      segment.size=0.4,
      box.padding=0.05
    ) +
    geom_point(aes(colour=focus,fill=focus),shape=21,size=2) +
    theme[["solarfacet"]]() +
    facet_wrap("key",scales="free") +
    labs(
      title="Shots / Passes",
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(limit=c(0,1)) +
    scale_y_continuous() +
    scale_colour_manual(values=c("TRUE"=colour[["sfc"]][["black"]],"FALSE"=colour[["sfc"]][["grey"]])) +
    scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["light"]],"FALSE"=colour[["sfc"]][["grey"]]))
  
  plots$gd <-
    data$fbref$players %>%
    filter(season %in% !!season) %>%
    filter(squad %in% !!squad) %>%
    select(player,min=playing_time_min,team_gls=`team_success_+-`,team_xg=`team_success_xg_xg+-`) %>%
    # mutate(team_gls=90*team_gls/min,team_xg=90*team_xg/min) %>%
    make_long_data(levels=c("team_gls","team_xg"),labels=c("Goals +/-","Expected Goals +/-")) %>%
    mutate(PM=ifelse(n>=0,TRUE,FALSE)) %>%
    ggplot(aes(x=0,y=n)) +
    geom_text_repel(
      aes(label=player),
      size=2.5,
      nudge_x=0.4,
      direction="y",
      hjust=0,
      segment.size=0.4,
      box.padding=0.05
    ) +
    geom_point(aes(colour=PM,fill=PM),shape=23,size=3,colour="black") +
    theme[["solarfacet"]]() +
    facet_wrap("key",scales="free") +
    labs(
      title="On-pitch goal difference",
      x=element_blank(),
      y=element_blank(),
      caption=glue("Goals/xG scored - Goals/xG conceded while each player is on the pitch")
    ) +
    scale_x_continuous(limit=c(0,1),breaks=seq(-50,50,1)) +
    scale_y_continuous(breaks=seq(-50,50,1)) +
    scale_fill_manual(values=c("TRUE"=colour[["medium"]][[3]],"FALSE"=colour[["medium"]][[8]]))
  
  plots$passescompleted <-
    data$fbref$players %>%
    filter(season %in% !!season) %>%
    filter(squad %in% !!squad) %>%
    select(player,short_cmp,medium_cmp,long_cmp) %>%
    make_long_data(levels=c("short_cmp","medium_cmp","long_cmp"),labels=c("Short (<5 yards)","Medium (5-25 yards)","Long (>25 yards)")) %>%
    # mutate(focus=case_when(percent_rank(n)>0.4 ~ TRUE,
    #                        TRUE ~ FALSE)) %>%
    mutate(focus=case_when(
      n==0 ~ FALSE,
      min_rank(desc(n))<=15 ~ TRUE,
      TRUE ~ FALSE)) %>%
    ggplot(aes(x=0,y=n)) +
    geom_text_repel(
      aes(label=ifelse(focus,player,"")),
      size=rel(3),
      nudge_x=0.3,
      direction="y",
      hjust=0,
      segment.size=0.4,
      box.padding=0.05
    ) +
    geom_point(aes(colour=focus,fill=focus),shape=21,size=2) +
    theme[["solarfacet"]]() +
    facet_wrap("key",scales="free") +
    labs(
      title="Completed passes",
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(limit=c(0,1)) +
    scale_y_continuous() +
    scale_colour_manual(values=c("TRUE"=colour[["sfc"]][["black"]],"FALSE"=colour[["sfc"]][["grey"]])) +
    scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["light"]],"FALSE"=colour[["sfc"]][["grey"]]))
  
  plots$passfootedness <-
    data$fbref$players %>%
    filter(season %in% !!season) %>%
    filter(squad %in% !!squad) %>%
    filter(body_parts_left+body_parts_right>0) %>%
    mutate(body_parts_tot=body_parts_left+body_parts_right) %>%
    mutate(body_parts_max=ifelse(body_parts_left>=body_parts_right,body_parts_left,body_parts_right)) %>%
    mutate(body_parts_min=ifelse(body_parts_left<body_parts_right,body_parts_left,body_parts_right)) %>%
    mutate(body_parts_ratio=(body_parts_max/body_parts_tot)) %>%
    mutate(body_parts_main=ifelse(body_parts_left>=body_parts_right,"Left","Right")) %>%
    mutate(body_parts_other=ifelse(body_parts_left<body_parts_right,"Left","Right")) %>%
    mutate(player=fct_reorder(player,desc(body_parts_ratio))) %>%
    ggplot(aes(y=player)) +
    geom_segment(aes(x=0,xend=body_parts_max,y=player,yend=player,colour=body_parts_main),size=2.5,alpha=0.8) +
    geom_segment(aes(x=0,xend=-body_parts_min,y=player,yend=player,colour=body_parts_other),size=2.5,alpha=0.8) +
    geom_label(aes(x=0,y=player,label=sprintf("%2.0f%%",100*body_parts_ratio)),size=1.6,label.padding=unit(0.16, "lines"),label.r = unit(0.08, "lines")) +
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
  # browser()
  plots$sca <-
    data$fbref$players %>%
    filter(season %in% !!season) %>%
    filter(squad %in% !!squad) %>%
    select(player,sca=sca_sca,sca_passlive=sca_types_passlive,sca_passdead=sca_types_passdead,
           sca_drib=sca_types_drib,sca_sh=sca_types_sh,sca_fld=sca_types_fld) %>%
    mutate(sca_other=sca_drib+sca_sh+sca_fld) %>%
    make_long_data(levels=c("sca_passlive","sca_passdead","sca_other"),labels=c("Open play pass","Dead ball pass","Dribble / Shot / Fouled")) %>%
    mutate(focus=case_when(
      n==0 ~ FALSE,
      # (key=="Open play pass" & min_rank(desc(n))<=15) ~ TRUE,
      # (key=="Dead ball pass" & min_rank(desc(n))<=2) ~ TRUE,
      # (key=="Dribble / Shot / Fouled" & min_rank(desc(n))<=8) ~ TRUE,
      min_rank(desc(n))<=15 ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    ggplot(aes(x=0,y=n)) +
    geom_text_repel(
      aes(label=ifelse(focus,player,"")),
      size=rel(2.5),
      nudge_x=0.3,
      direction="y",
      hjust=0,
      segment.size=0.4,
      box.padding=0.05
    ) +
    geom_point(aes(colour=focus,fill=focus),shape=21,size=2) +
    theme[["solarfacet"]]() +
    theme(
      # plot.title=element_text(),
    ) +
    facet_wrap("key",scales="free") +
    labs(
      title="Shot Creating Actions",
      x=element_blank(),
      y=element_blank(),
      caption=glue("Shot Creating Actions are the two actions directly leading to a shot")
    ) +
    scale_x_continuous(limit=c(0,1)) +
    scale_y_continuous() +
    scale_colour_manual(values=c("TRUE"=colour[["sfc"]][["black"]],"FALSE"=colour[["sfc"]][["grey"]])) +
    scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["light"]],"FALSE"=colour[["sfc"]][["grey"]]))
  
  plots$xgtrendsmooth <-
    data$fbref$matches %>%
    make_long_matches() %>%
    filter(season %in% !!season) %>%
    filter(squad %in% !!squad) %>%
    mutate(season=case_when(
      date>as.Date("2019-08-01") & date<as.Date("2020-03-31") ~ "2019-20 part 1",
      date>as.Date("2020-04-01") & date<as.Date("2020-07-30") ~ "2019-20 part 2",
      TRUE ~ season)) %>%
    filter(!is.na(homegls)) %>%
    mutate(shortha=ifelse(ha=="home","H","A")) %>%
    mutate(match=glue::glue("{opposition} {shortha} {glsf}-{glsa}")) %>%
    mutate(match=reorder_within(match, date, season)) %>%
    ggplot(aes(x=match)) +
    geom_point(aes(y=xgf),size=1,colour="darkred",fill="darkred",alpha=0.5,shape=23) +
    geom_spline(aes(y=xgf,group=season),spar=0.5,colour="darkred",linetype="longdash",size=0.7) +
    geom_point(aes(y=xga),size=1,colour="royalblue",fill="royalblue",alpha=0.5,shape=23) +
    geom_spline(aes(y=xga,group=season),spar=0.5,colour="royalblue",linetype="longdash",size=0.7) +
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
      title=glue("{squad} <b style='color:darkred'>attack</b> / <b style='color:royalblue'>defence</b> xG trend"),
      x=element_blank(),
      y=glue("Expected goals <b style='color:darkred'>for</b> / <b style='color:royalblue'>against</b>")
    ) +
    scale_x_reordered() +
    scale_y_continuous(limits=c(0,NA),expand=expansion(add=c(0,0.1))) +
    facet_grid(cols=vars(season), space="free", scales="free_x")
  
  plots$xgtrendmva <-
    data$fbref$matches %>%
    make_long_matches() %>%
    filter(squad %in% !!squad) %>%
    mutate(xgf_mva=get_mva(xgf)) %>%
    mutate(xga_mva=get_mva(xga)) %>%
    filter(season %in% !!season) %>%
    # filter(date>=as.Date("2020-01-01")) %>%
    mutate(season=case_when(
      date>as.Date("2019-08-01") & date<as.Date("2020-03-31") ~ "2019-20 part 1",
      date>as.Date("2020-04-01") & date<as.Date("2020-07-30") ~ "2019-20 part 2",
      TRUE ~ season)) %>%
    filter(!is.na(homegls)) %>%
    mutate(shortha=ifelse(ha=="home","H","A")) %>%
    mutate(match=glue::glue("{opposition} {shortha} {glsf}-{glsa}")) %>%
    mutate(match=reorder_within(match, date, season)) %>%
    ggplot(aes(x=match)) +
    geom_point(aes(y=xgf),size=1,colour="darkred",fill="darkred",alpha=0.5,shape=23) +
    geom_line(aes(y=xgf_mva,group=season),colour="darkred",linetype="longdash",size=0.7) +
    geom_point(aes(y=xga),size=1,colour="royalblue",fill="royalblue",alpha=0.5,shape=23) +
    geom_line(aes(y=xga_mva,group=season),colour="royalblue",linetype="longdash",size=0.7) +
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
      title=glue("{squad} <b style='color:darkred'>attack</b> / <b style='color:royalblue'>defence</b> xG trend"),
      x=element_blank(),
      y=glue("Expected goals <b style='color:darkred'>for</b> / <b style='color:royalblue'>against</b>")
    ) +
    scale_x_reordered() +
    scale_y_continuous(limits=c(0,NA),expand=expansion(add=c(0,0.1))) +
    facet_grid(cols=vars(season), space="free", scales="free_x")
  
  plots$xgsegment <-
    data$fbref$matches %>%
    make_long_matches() %>%
    filter(season %in% !!season) %>%
    filter(squad %in% !!squad) %>%
    # filter(date>=as.Date("2020-01-01")) %>%
    mutate(season=case_when(
      date>as.Date("2019-08-01") & date<as.Date("2020-03-31") ~ "2019-20 part 1",
      date>as.Date("2020-04-01") & date<as.Date("2020-07-30") ~ "2019-20 part 2",
      TRUE ~ season)) %>%
    filter(!is.na(homegls)) %>%
    mutate(shortha=ifelse(ha=="home","H","A")) %>%
    mutate(match=glue("{opposition} {shortha} {glsf}-{glsa}")) %>%
    mutate(match=reorder_within(match, desc(date), season)) %>%
    ggplot(aes(y=match)) +
    geom_segment(aes(x=0,xend=xgf,y=match,yend=match),colour=colour[["sfc"]][["light"]],size=2) +
    geom_segment(aes(x=0,xend=-xga,y=match,yend=match),colour=colour[["medium"]][[1]],size=2) +
    theme[["solar"]]() +
    theme(
      plot.title=element_markdown(),
      axis.text.x=element_text(),
      axis.text.y=element_text(size=6),
      strip.text=element_blank()
    ) +
    labs(
      title=glue("<b style='color:#265DAB'>Opposition xG</b> | <b style='color:#D71920'>{squad} xG</b>"),
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(breaks=seq(-10,10,1),labels=abs(seq(-10,10,1)),expand=expansion(add=c(0.1,1))) +
    scale_y_reordered() +
    facet_grid(rows=vars(season), space="free", scales="free_y")
  
  plots$subs <-
    data$fbref$events %>%
    filter(type=="Substitute") %>%
    mutate(h_a=team) %>%
    left_join(data$fbref$matches %>% 
                select(match_key, homeglsf=homegls, awayglsf=awaygls, date) %>%
                unique,
              by="match_key") %>%
    mutate(state=case_when(
      team=="Away" ~ (state * -1),
      TRUE ~ state
    )) %>%
    mutate(opposition=case_when(
      team=="Home" ~ away,
      TRUE ~ home
    )) %>%
    mutate(team=case_when(
      team=="Home" ~ home,
      TRUE ~ away
    )) %>%
    mutate(state2=case_when(
      state<0 ~ "Losing",
      state==0 ~ "Drawing",
      TRUE ~ "Winning"
    )) %>%
    type_convert %>%
    mutate(opposition=fct_reorder(opposition,desc(date))) %>%
    filter(season %in% !!season) %>%
    filter(team %in% !!squad) %>%
    # filter(date>=lubridate::as_date("2018-12-7")) %>%
    mutate(match=glue::glue("{opposition} {h_a} {homeglsf}-{awayglsf}")) %>%
    mutate(match=reorder_within(match, desc(date), season)) %>%
    ggplot(aes(x=time,y=match,fill=state2)) +
    geom_beeswarm(size=2.5,shape=23,colour="black",groupOnX=FALSE,priority="descending",alpha=0.75) +
    theme[["solar"]]() +
    facet_grid(season ~ ., space="free", scales="free_y") +
    theme(
      axis.text.y=element_text(size=4.5),
      # panel.grid.major.y=element_blank(),
      # panel.grid.minor=element_blank(),
      # strip.text.y=element_text(colour="black",face="bold",size=7,angle=0,hjust=0),
      strip.text.y=element_blank(),
      # legend.position="top",
      # legend.title=element_blank(),
      # legend.text=element_text(colour="black"),
      # legend.key=element_rect(fill=NA)
    ) +
    labs(
      title=glue("{squad} substitutions"),
      x=element_blank(),
      y=element_blank()
      # caption="data: FBref | Project Restart (5 allowed subs) in blue | 5x first half subs not shown"
    ) +
    scale_x_continuous(breaks=seq(0,130,5),expand=expansion(add=2)) +
    scale_y_reordered(expand=expansion(add=c(1.2,1.6)),position = "right") +
    scale_fill_manual(values=c("Winning"=colour[["medium"]][[3]],"Drawing"=colour[["medium"]][[1]],"Losing"=colour[["medium"]][[8]]))

  plots$subs3 <-
    data$fbref$matches %>%
    filter(!is.na(homegls)) %>%
    select(match_key=code,season,date,home,away,homeglsft=homegls,awayglsft=awaygls) %>%
    unique() %>%
    left_join(data$fbref$events %>%
                filter(type=="Substitute") %>%
                select(match_key,half,time,team,homegls,awaygls,state) %>%
                rename(home_away=team),
              by="match_key") %>%
    type_convert() %>%
    mutate(state=case_when(
      home_away=="Away" ~ (state * -1),
      TRUE ~ state
    )) %>%
    mutate(team=case_when(
      home_away=="Home" ~ home,
      TRUE ~ away
    )) %>%
    mutate(opposition=case_when(
      home_away=="Home" ~ away,
      TRUE ~ home
    )) %>%
    mutate(period=case_when(
      half==1 ~ "First Half",
      half==2 & time==46 ~ "Half Time",
      half==2 ~ "Second Half",
      FALSE ~ "Unused",
      TRUE ~ "Other"
    )) %>%
    mutate(time=case_when(
      period=="First Half" ~ -2,
      period=="Half Time" ~ -1,
      period=="Unused" ~ -3,
      TRUE ~ time
    )) %>%
    mutate(state2=case_when(
      state>0 ~ "Winning",
      state<0 ~ "Losing",
      TRUE ~ "Drawing"
    )) %>%
    mutate(home_away=case_when(
      home_away=="Home" ~ "H",
      TRUE ~ "A"
    )) %>%
    mutate(teamglsft=ifelse(home_away=="H",homeglsft,awayglsft)) %>%
    mutate(oppglsft=ifelse(home_away=="H",awayglsft,homeglsft)) %>%
    filter(season %in% !!season) %>%
    filter(team %in% !!squad) %>%
    nest_by(match_key,date,season,home,away,homeglsft,awayglsft,teamglsft,oppglsft,team,opposition,home_away) %>%
    mutate(subs_available=case_when(
      date>=lubridate::as_date("2020-4-1") & date<lubridate::as_date("2020-9-1") ~ 5,
      TRUE ~ 3
    )) %>%
    mutate(subs_used=dim(data)[1]) %>%
    ungroup() %>%
    mutate(data=pmap(list(.$data,subs_available,subs_used),possibly(get_unused_subs, otherwise=NA))) %>%
    unnest(cols=data) %>%
    mutate(season=case_when(
      date>as.Date("2019-08-01") & date<as.Date("2020-03-31") ~ "2019-20 part 1",
      date>as.Date("2020-04-01") & date<as.Date("2020-07-30") ~ "2019-20 part 2",
      TRUE ~ season)) %>%
    mutate(match=glue("{teamglsft}-{oppglsft} {opposition} {home_away}")) %>%
    mutate(match=reorder_within(match, desc(date), season)) %>%
    # filter(date>=lubridate::as_date("2018-12-7")) %>%
    # filter(date>=lubridate::as_date("2020-1-1")) %>%
    ggplot(aes(x=time,y=match,fill=state2)) +
    geom_beeswarm(size=2.5,shape=23,cex=2,colour="black",groupOnX=FALSE,priority="descending",alpha=0.75) +
    # geom_blank(data=data.frame(time=c(45.5,94.5),match=c(NA,NA),state2=c(NA,NA),period=c("Second Half","Second Half"))) +
    theme[["solar"]]() +
    facet_grid(season ~ period, space="free", scales="free") +
    theme(
      axis.text.x=element_text(size=5),
      axis.text.y=element_text(size=5),
      strip.text=element_blank(),
      plot.caption=element_markdown()
    ) +
    labs(
      title=glue("{squad} substitutions"),
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
  
  save_plots(plots_logo,path=here("plots","team"))
}
