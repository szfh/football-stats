source(here("R","plot","plot-utils.R"))

plot_league <- function(data,league="EPL",season="2020-21"){
  plots <- list()
  
  # Premier League player plots
  plots$glsxg <-
    data$fbref$players %>%
    filter(season %in% !!season) %>%
    select(player,squad,gls=standard_gls,pk=performance_pk,npxg=expected_npxg) %>%
    mutate(npgls=gls-pk) %>%
    group_by(player,squad) %>%
    summarise(npgls=sum(npgls,na.rm=TRUE),npxg=sum(npxg,na.rm=TRUE)) %>%
    make_long_data(levels=c("npgls","npxg"),labels=c("Goals","Expected Goals")) %>%
    mutate(focus=case_when(min_rank(desc(n))<=10 ~ TRUE,
                           TRUE ~ FALSE)) %>%
    ggplot(aes(x=0.05,y=n,alpha=focus)) +
    geom_text_repel(
      aes(label=ifelse(focus,player,"")),
      size=2.5,
      nudge_x=0.3,
      direction="y",
      hjust=0,
      segment.size=0.4,
      box.padding=0.05
      # position=position_jitterdodge(jitter.width=0,jitter.height=0.17,dodge.width=0.3,seed=2)
    ) +
    geom_point(aes(fill=squad),shape=21,size=3,position=position_jitterdodge(jitter.width=0,jitter.height=0,dodge.width=0.04,seed=2)) +
    theme[["solarfacet"]]() +
    facet_wrap("key",scales="free") +
    labs(
      title="Expected Goals (penalties excluded)",
      x=element_blank(),
      y=element_blank()) +
    scale_x_continuous(limit=c(0,1)) +
    scale_y_continuous() +
    scale_fill_manual(values=palette[["epl"]]()) +
    scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.1))
  
  plots$psxg <-
    data$fbref$squad %>%
    filter(season %in% !!season) %>%
    select(squad,vs,gls=standard_gls,psxg=expected_psxg,psxgsot=expected_psxgsot,psxgpm=`expected_psxg+-`) %>%
    filter(vs) %>%
    mutate(psxgpm=-psxgpm) %>%
    mutate(squad=case_when(
      vs ~ str_sub(squad,4),
      TRUE ~ squad
    )) %>%
    mutate(PM=ifelse(psxgpm>=0,TRUE,FALSE)) %>%
    mutate(squad=fct_reorder(squad,psxgpm)) %>%
    glimpse %>%
    ggplot(aes(x=psxgpm,y=squad,colour=PM)) +
    geom_segment(aes(y=squad,yend=squad,x=0,xend=psxgpm),size=3.5,alpha=0.8) +
    theme[["solar"]]() +
    labs(
      title="Post-shot expected goals",
      x=element_blank(),
      y=element_blank()) +
    # annotate("text",label="Goalkeepers underperform\nagainst these teams",fontface="bold",hjust="right",size=3,x=-0.6,y=17) +
    # annotate("text",label="Goalkeepers overperform\nagainst these teams",fontface="bold",hjust="left",size=3,x=0.6,y=6) +
    scale_x_continuous(breaks=seq(-200,200,2),expand=expansion(add=c(0.1))) +
    scale_colour_manual(values=c("TRUE"=colour[["medium"]][[3]],"FALSE"=colour[["medium"]][[8]]))
  
  plots$playerxgxa <-
    data$fbref$players %>%
    filter(season %in% !!season) %>%
    select(player,squad,npxg=expected_npxg,xa=expected_xa) %>%
    make_long_data(levels=c("npxg","xa"),labels=c("Expected Goals","Expected Assists")) %>%
    mutate(focus=case_when(min_rank(desc(n))<=20 ~ TRUE,
                           TRUE ~ FALSE)) %>%
    ggplot(aes(x=0.05,y=n,alpha=focus)) +
    geom_text_repel(
      aes(label=ifelse(focus,player,"")),
      size=2.5,
      nudge_x=0.3,
      direction="y",
      hjust=0,
      segment.size=0.4,
      box.padding=0.05
    ) +
    geom_point(aes(fill=squad),shape=21,size=3,position=position_jitterdodge(jitter.width=0,jitter.height=0,dodge.width=0.04,seed=2)) +
    theme[["solarfacet"]]() +
    facet_wrap("key",scales="free") +
    labs(
      title="Expected Goals (penalties excluded)",
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(limit=c(0,1)) +
    scale_y_continuous() +
    scale_fill_manual(values=palette[["epl"]]()) +
    scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.2))
  
  plots$playercomppasses <-
    data$fbref$players %>%
    filter(season %in% !!season) %>%
    select(player,squad,short_cmp,medium_cmp,long_cmp) %>%
    make_long_data(levels=c("short_cmp","medium_cmp","long_cmp"),labels=c("Short (<5 yards)","Medium (5-25 yards)","Long (>25 yards)")) %>%
    group_by(key,squad) %>%
    mutate(focus=ifelse(min_rank(desc(n))==1,TRUE,FALSE)) %>%
    ggplot(aes(x=0,y=n)) +
    geom_text_repel(
      aes(label=ifelse(focus,player,"")),
      size=2.5,
      nudge_x=0.3,
      direction="y",
      hjust=0,
      segment.size=0.4,
      segment.alpha=0.8,
      box.padding=0.05
    ) +
    geom_point(aes(fill=squad,alpha=focus),shape=21,size=2,position=position_jitterdodge(jitter.width=0,jitter.height=0.2,dodge.width=0)) +
    theme[["solarfacet"]]() +
    facet_wrap("key",scales="free") +
    labs(
      title="Completed passes",
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(limit=c(0,1)) +
    scale_y_continuous() +
    scale_fill_manual(values=palette[["epl"]]()) +
    scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.2))
  
  plots$playernogoals <-
    data$fbref$players %>%
    filter(season %in% !!season) %>%
    select(player,squad,sh=standard_sh,gls=standard_gls,npxg=expected_npxg) %>%
    filter(gls==0) %>%
    mutate(focus=case_when(percent_rank(sh)>0.92 ~ TRUE,
                           percent_rank(npxg)>0.92 ~ TRUE,
                           TRUE ~ FALSE
    )) %>%
    mutate(squad=ifelse(focus,squad,"Other")) %>%
    ggplot(aes(x=npxg,y=sh)) +
    geom_text_repel(aes(label=ifelse(focus,player,"")),size=2,position=position_jitter(width=0.05,height=0.2,seed=2)) +
    geom_point(aes(fill=squad,alpha=focus),size=2,shape=23,colour="black",position=position_jitter(width=0.05,height=0.2,seed=2)) +
    theme[["solar"]]() +
    labs(
      title="Who hasn't scored yet?",
      x="Expected goals",
      y="Shots"
    ) +
    scale_fill_manual(values=palette[["epl"]]()) +
    scale_x_continuous(breaks=seq(0,50,1),expand=expansion(add=c(0,0.2))) +
    scale_y_continuous(breaks=seq(0,200,5),expand=expansion(add=c(0,2))) +
    scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.2))
  
  # plots$playernogoalsallseasons <-
  #   data$fbref$players %>%
  #   select(player,squad,season,sh=standard_sh,gls=standard_gls,npxg=expected_npxg) %>%
  #   filter(gls==0) %>%
  #   mutate(focus=case_when(percent_rank(sh)>0.96 ~ TRUE,
  #                          percent_rank(npxg)>0.94 ~ TRUE,
  #                          TRUE ~ FALSE
  #   )) %>%
  #   mutate(season=ifelse(focus,season,"Other")) %>%
  #   ggplot(aes(x=npxg,y=sh)) +
  #   geom_text_repel(aes(label=ifelse(focus,player,"")),size=1.5,position=position_jitter(width=0.05,height=0.2,seed=2)) +
  #   geom_point(aes(fill=season,alpha=focus),size=2,shape=23,colour="black",position=position_jitter(width=0.05,height=0.2,seed=2)) +
  #   theme[["solar"]]() +
  #   theme(
  #     legend.position=c(0.9,0.18),
  #     legend.title=element_blank(),
  #     legend.text=element_text(colour="black"),
  #     legend.key=element_rect(fill=NA)
  #   ) + 
  #   labs(
  #     title="Who didn't score all season?",
  #     x="Expected goals",
  #     y="Shots"
  #   ) +
  #   scale_fill_manual(breaks=c("2017-18","2018-19","2019-20"),values=c("2017-18"=colour$dark[3],"2018-19"=colour$dark[1],"2019-20"=colour$dark[8],"Other"="lightgrey")) +
  #   scale_x_continuous(breaks=seq(0,50,1),expand=expansion(add=c(0,0.2))) +
  #   scale_y_continuous(breaks=seq(0,200,5),expand=expansion(add=c(0,2))) +
  #   scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.2),guide="none")
  
  # Premier League team plots
  
  plots$squadxg1 <-
    data$fbref$squad %>%
    filter(season %in% !!season) %>%
    select(squad,xg,xga) %>%
    mutate(xga=-xga) %>%
    make_long_data(levels=c("xg","xga"),labels=c("xG For","xG Against")) %>%
    ggplot(aes(x=0,y=n)) +
    geom_text_repel(
      aes(label=squad),
      size=2.5,
      nudge_x=0.5,
      direction="y",
      hjust=0,
      segment.size=0.4,
      segment.alpha=0.8,
      box.padding=0.05
    ) +
    geom_point(aes(fill=squad),size=3,shape=21,colour="black") +
    theme[["solarfacet"]]() +
    facet_wrap("key",scales="free") +
    labs(
      title=element_blank(),
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(limit=c(0,1)) +
    scale_y_continuous(breaks=seq(-100,100,5),labels=abs(seq(-100,100,5)),expand=expansion(add=c(2))) +
    scale_fill_manual(values=palette[["epl"]]())
  
  plots$squadxg2 <-
    data$fbref$squad %>%
    filter(!vs) %>%
    filter(season %in% !!season) %>%
    select(squad,xg=expected_npxg,xga) %>%
    ggplot(aes(x=xg,y=xga)) +
    geom_text_repel(aes(label=squad),size=2) +
    geom_point(aes(fill=squad),shape=23,size=2.5) +
    theme[["solar"]]() +
    labs(
      title="Expected goals",
      x="xG for",
      y="xG against"
    ) +
    scale_x_continuous(breaks=seq(0,100,5),expand=expansion(add=c(0.5))) +
    scale_y_reverse(breaks=seq(0,100,5),expand=expansion(add=c(0.5))) +
    scale_fill_manual(values=palette[["epl"]]())
  
  plots$squadxgd <-
    data$fbref$squad %>%
    filter(season %in% !!season) %>%
    select(squad,gd=gdiff,xgd=xgdiff) %>%
    make_long_data(levels=c("gd","xgd"),labels=c("Goal difference","Expected goal difference")) %>%
    ggplot(aes(x=0,y=n)) +
    geom_text_repel(
      aes(label=squad),
      size=2,
      nudge_x=0.5,
      direction="y",
      hjust=0,
      segment.size=0.4,
      segment.alpha=0.8,
      box.padding=0.05
    ) +
    geom_point(aes(fill=squad),size=3,shape=21,colour="black") +
    theme[["solarfacet"]]() +
    facet_wrap("key",scales="free") +
    labs(
      title=element_blank(),
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(limit=c(0,1)) +
    scale_y_continuous(breaks=seq(-100,100,5),expand=expansion(add=3)) +
    scale_fill_manual(values=palette[["epl"]]())
  
  plots$playeratt3rdactions <-
    data$fbref$players %>%
    select(player,season,squad,touch=touches_att_3rd,pressure=pressures_att_3rd,tackle=tackles_att_3rd) %>%
    filter(season %in% !!season) %>%
    make_long_data(levels=c("touch","pressure","tackle"),labels=c("Touches","Pressures","Tackles")) %>%
    group_by(key,squad) %>%
    mutate(focus=ifelse(min_rank(desc(n))==1,TRUE,FALSE)) %>%
    ggplot(aes(x=0,y=n)) +
    geom_text_repel(
      aes(label=ifelse(focus,player,"")),
      size=2,
      nudge_x=0.3,
      direction="y",
      hjust=0,
      segment.size=0.4,
      segment.alpha=0.8,
      box.padding=0.05
    ) +
    geom_point(aes(fill=squad,alpha=focus),shape=21,size=2,position=position_jitter(width=0,height=0.1)) +
    theme[["solarfacet"]]() +
    facet_wrap("key",scales="free") +
    labs(
      title="2019-20 attacking third actions",
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(limit=c(0,1)) +
    scale_y_continuous() +
    scale_fill_manual(values=palette[["epl"]]()) +
    scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.1))
  
  plots$pressures <-
    data$fbref$squad %>%
    filter(season %in% !!season) %>%select(squad,vs,pass_types_live,pressures_press,pressures_def_3rd:pressures_att_3rd,touches_live,touches_def_3rd:touches_att_3rd) %>%
    mutate(squad=case_when(
      vs ~ str_sub(squad,4),
      TRUE ~ squad
    )) %>%
    mutate(vs=case_when(
      vs ~ "vs",
      TRUE ~ ""
    )) %>%
    pivot_wider(names_from=vs,values_from=pass_types_live:touches_att_3rd) %>%
    # glimpse %>%
    mutate(
      def3rd=pressures_def_3rd_/touches_att_3rd_vs,
      mid3rd=pressures_mid_3rd_/touches_mid_3rd_vs,
      att3rd=pressures_att_3rd_/touches_def_3rd_vs
    ) %>%
    make_long_data(levels=c("def3rd","mid3rd","att3rd"),labels=c("Defensive Third","Middle Third","Attacking Third")) %>%
    ggplot(aes(x=0,y=n)) +
    geom_text_repel(
      aes(label=squad),
      size=2.5,
      nudge_x=0.5,
      direction="y",
      hjust=0,
      segment.size=0.4,
      segment.alpha=0.8,
      box.padding=0.05
    ) +
    geom_point(aes(fill=squad),size=3,shape=23,colour="black") +
    theme[["solarfacet"]]() +
    facet_wrap("key",scales="free") +
    labs(
      title="Press intensity",
      x=element_blank(),
      y=element_blank(),
      caption="percentage of opposition ball possessions put under pressure | control-dribble-offload = single possession"
    ) +
    scale_x_continuous(limit=c(0,1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values=palette[["epl"]]())
  
  plots$ppda <-
    data$fbref$squad %>%
    filter(season %in% !!season) %>%
    select(squad,vs,pass_types_live,pressures_press:pressures_att_3rd,touches_touches,touches_live,touches_def_3rd:touches_att_3rd) %>%
    mutate(squad=case_when(
      vs ~ str_sub(squad,4),
      TRUE ~ squad
    )) %>%
    mutate(vs=case_when(
      vs ~ "vs",
      TRUE ~ ""
    )) %>%
    pivot_wider(names_from=vs,values_from=pass_types_live:touches_att_3rd) %>%
    ggplot(aes(x=pressures_press_,y=pass_types_live_vs,)) +
    geom_point(aes(fill=squad),shape=21,size=3.5,alpha=0.8) +
    geom_text_repel(aes(label=squad),size=2.5) +
    theme[["solar"]]() +
    labs(
      title="Passes per Pressure",
      x="Pressures",
      y="Opposition Passes"
    ) +
    scale_fill_manual(values=palette[["epl"]]()) +
    scale_x_continuous(breaks=seq(0,2000,100),expand=expansion(add=c(20))) +
    scale_y_reverse(breaks=seq(0,5000,500),expand=expansion(add=c(50)))
  
  plots$tpda <-
    data$fbref$squad %>%
    filter(season %in% !!season) %>%
    select(squad,vs,pressures_def_3rd:pressures_att_3rd,touches_touches,touches_live,touches_def_3rd:touches_att_3rd) %>%
    mutate(squad=case_when(
      vs ~ str_sub(squad,4),
      TRUE ~ squad
    )) %>%
    mutate(vs=case_when(
      vs ~ "vs",
      TRUE ~ ""
    )) %>%
    pivot_wider(names_from=vs,values_from=pressures_def_3rd:touches_att_3rd) %>%
    ggplot(aes(x=pressures_mid_3rd_+pressures_att_3rd_,y=touches_def_3rd_vs+touches_mid_3rd_vs)) +
    geom_point(aes(fill=squad),shape=21,size=3.5,alpha=0.8) +
    geom_text_repel(aes(label=squad),size=2.5) +
    theme[["solar"]]() +
    labs(
      title="Touches per Pressure",
      x="Pressures (exc defensive third)",
      y="Opposition Touches (exc attacking third)"
    ) +
    scale_fill_manual(values=palette[["epl"]]()) +
    scale_x_continuous(breaks=seq(0,1000,100),expand=expansion(add=c(20))) +
    scale_y_reverse(breaks=seq(0,5000,500),expand=expansion(add=c(50)))
  
  # labels <- tribble(
  #   ~"squad",~"n",~"key",~"pos",~"label",
  #   "Leicester City",2.5,"Defensive Third","FW","Leicester's FWs\nrelieve pressure by\nstaying forward",
  #   "Leeds United",28.5,"Attacking Third","MF","Leeds' midfielders\npress everywhere",
  #   "Brighton",13.5,"Attacking Third","DF","High full back teams\nwill show up here\n(this is Lamptey + March)",
  #   "Arsenal",17.5,"Middle Third","MF","Arsenal are\nthe least\nactive in\nmidfield"
  # ) %>%
  #   mutate(across(c(squad,key,pos),factor))
  
  plots$pressposition <-
    data$fbref$players %>%
    filter(season %in% !!season) %>%
    select(player,squad,pos1,pos2,mp=playing_time_mp,min=playing_time_min,press=pressures_press,def3rd=pressures_def_3rd,mid3rd=pressures_mid_3rd,att3rd=pressures_att_3rd) %>%
    mutate(squad=fct_reorder(squad,press)) %>%
    mutate(pos=factor(pos1,levels=c("FW","MF","DF","GK"))) %>%
    filter(pos!="GK") %>%
    mutate(across(c("def3rd","mid3rd","att3rd"), ~ ./mp)) %>%
    make_long_data(levels=c("def3rd","mid3rd","att3rd"),labels=c("Defensive Third","Middle Third","Attacking Third")) %>%
    group_by(squad,pos,key) %>%
    summarise(n=sum(n,na.rm=TRUE)) %>%
    ungroup() %>%
    left_join(
      data$fbref$squad %>%
        filter(!vs) %>%
        filter(season=="2020-21") %>%
        select(squad) %>%
        rownames_to_column("position") %>%
        mutate(position=(as.numeric(position)))
    ) %>%
    mutate(squad=fct_reorder(squad,desc(position))) %>%
    ggplot() +
    geom_point(aes(x=n,y=squad,fill=squad),shape=21,size=1.5) +
    # geom_text_repel(data=labels[1,], aes(x=n,y=squad,label=label),fontface="bold",size=1.6,nudge_x=35,segment.size=0.4,box.padding=0.01,min.segment.length = 0) +
    # geom_text_repel(data=labels[2,], aes(x=n,y=squad,label=label),fontface="bold",size=1.6,nudge_x=-6,nudge_y=4,segment.size=0.4,box.padding=0.01,min.segment.length = 0) +
    # geom_text_repel(data=labels[3,], aes(x=n,y=squad,label=label),fontface="bold",size=1.6,nudge_x=11,segment.size=0.4,box.padding=0.01,min.segment.length = 0) +
    # geom_text_repel(data=labels[4,], aes(x=n,y=squad,label=label),fontface="bold",size=1.6,nudge_x=-3,nudge_y=8,segment.size=0.4,box.padding=0.01,min.segment.length = 0) +
    facet_grid(rows=vars(pos),cols=vars(key),scales="free_x") +
    theme[["solar"]]() +
    theme(
      plot.title=element_text(),
      axis.line=element_blank(),
      axis.text=element_text(size=3.5),
      strip.text.y=element_text(angle=0)
    ) +
    labs(
      title="Pressures per match -\nPitch location and player position",
      x=element_blank(),
      y=element_blank()
    ) +
    scale_x_continuous(expand=expansion(mult=0.025)) +
    scale_y_discrete(expand=expansion(add=1)) +
    scale_fill_manual(values=palette[["epl"]]())
  
  plots_logo <- 
    plots %>%
    add_logo(path=here("images","SB_Regular.png"),x=1,y=1,hjust=1.1,width=0.2) %>%
    add_logo(path=here("images","fbref.png"),x=0.88,y=1,hjust=1.1,width=0.29)
  
  save_plots(plots_logo,path=here("plots","league"))
}
