players %>%
  filter_season_team() %>%
  select(player,pos,min=playing_time_min,starts=starts_starts,subs=subs_subs,min_start=starts_mn_start,min_sub=subs_mn_sub) %>%
  filter_na("min") %>%
  mutate(
    min_start=min_start*starts,
    min_sub=min_sub*subs
  ) %>%
  mutate(
    pos=case_when(
      player %in% c("Kevin Danso","Jannik Vestergaard","Jan Bednarek","Jack Stephens","Maya Yoshida") ~ "CB",
      player %in% c("Ryan Bertrand","Cédric Soares","Yan Valery","Kyle Walker-Peters") ~ "FB",
      player %in% c("James Ward-Prowse","Pierre Højbjerg","Oriol Romeu","William Smallbone") ~ "DM",
      player %in% c("Nathan Redmond","Stuart Armstrong","Sofiane Boufal","Moussa Djenepo") ~ "AM",
      TRUE ~ pos)
  ) %>%
  mutate(pos=factor(pos,levels=c("GK","CB","FB","DM","AM","FW"))) %>%
  mutate(player=fct_reorder(player,min)) %>%
  ggplot(aes(x=min,y=player)) +
  geom_segment(aes(y=player,yend=player,x=0,xend=min_start),colour=colour[["sfc"]][["main"]],size=3.5,alpha=0.8) +
  geom_segment(aes(y=player,yend=player,x=min_start,xend=min),colour=colour[["sfc"]][["light"]],size=3.5,alpha=0.8) +
  theme[["solar"]]() +
  theme(
    plot.title=element_markdown(),
    axis.line=element_blank(),
    axis.text.x=element_text(size=rel(0.8),hjust=0.5),
    axis.text.y=element_text(size=rel(0.8)),
    strip.text.y=element_text(angle=0)
  ) +
  facet_grid(pos ~ ., space="free", scales="free_y") +
  labs(
    title=glue("League minutes (<b style='color:#D71920'>from start</b> / <b style='color:#ED5C5C'>from bench</b>)"),
    x=element_blank(),
    y=element_blank(),
    caption=caption[[1]]
  ) +
  scale_x_continuous(breaks=seq(0,90*38,180),expand=expansion(add=c(0,20)))
# ggsave(here("plots","SFC","Minutes.jpg"))

players %>%
  filter_season_team() %>%
  select(player,npxg=expected_npxg,xa=expected_xa) %>%
  filter_na(c("npxg","xa")) %>%
  mutate(focus=ifelse(npxg>=1|xa>=1,TRUE,FALSE)) %>%
  ggplot(aes(x=npxg,y=xa)) +
  geom_point(aes(fill=focus),shape=21,size=4,alpha=0.8,colour=colour[["sfc"]][["black"]]) +
  geom_text_repel(aes(label=ifelse(focus,player,"")),size=rel(4)) +
  theme[["solar"]]() +
  labs(title="Southampton xG/xA",
       x="Expected goals",
       y="Expected assists",
       caption=caption[[1]]) +
  scale_x_continuous(limits=c(0,NA),breaks=seq(0,30,1),expand=expansion(add=c(0,0.2))) +
  scale_y_continuous(limits=c(0,NA),breaks=seq(0,30,1),expand=expansion(add=c(0,0.2))) +
  scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["main"]],"FALSE"=colour[["sfc"]][["grey"]])) +
  coord_fixed()
# ggsave(here("plots","SFC","xGxA.jpg"))

players %>%
  filter_season_team() %>%
  select(player,sh=standard_sh,kp) %>%
  make_long_data(levels=c("sh","kp"),labels=c("Shot","Pass leading to shot")) %>%
  mutate(focus=case_when(percent_rank(n)>0.4 ~ TRUE,
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
  theme[["solar"]]() +
  theme(
    axis.line.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    panel.grid.major.x=element_blank()
  ) +
  facet_wrap("key",scales="free") +
  labs(
    title="Shots / Passes",
    x=element_blank(),
    y=element_blank(),
    caption=caption[[1]]) +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous() +
  scale_colour_manual(values=c("TRUE"=colour[["sfc"]][["black"]],"FALSE"=colour[["sfc"]][["grey"]])) +
  scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["light"]],"FALSE"=colour[["sfc"]][["grey"]]))
# ggsave(here("plots","SFC","ShotsKP.jpg"))

players %>%
  filter_season_team() %>%
  select(player,min=playing_time_min,team_gls=team_success..,team_xg=team_success.xg.xg..) %>%
  make_long_data(levels=c("team_gls","team_xg"),labels=c("Goals +/-","xG +/-")) %>%
  mutate(PM=ifelse(n>=0,TRUE,FALSE)) %>%
  ggplot(aes(x=0,y=n)) +
  geom_text_repel(
    aes(label=player),
    size=rel(3),
    nudge_x=0.4,
    direction="y",
    hjust=0,
    segment.size=0.4,
    box.padding=0.05
  ) +
  geom_point(aes(colour=PM,fill=PM),shape=21,size=3,colour="black") +
  theme[["solar"]]() +
  theme(
    axis.line.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    panel.grid.major.x=element_blank()
  ) +
  facet_wrap("key",scales="free") +
  labs(
    title="On-pitch goal difference (per 90 mins)",
    x=element_blank(),
    y=element_blank(),
    caption=glue("Goals/xG for - against while each player is on the pitch\n",caption[[1]])
  ) +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous() +
  scale_fill_manual(values=c("TRUE"=colour[["medium"]][[3]],"FALSE"=colour[["medium"]][[8]]))
# ggsave(here("plots","SFC","GD.jpg"),dpi=600)

players %>%
  filter_season_team() %>%
  select(player,short_cmp,medium_cmp,long_cmp) %>%
  make_long_data(levels=c("short_cmp","medium_cmp","long_cmp"),labels=c("Short (<5 yards)","Medium (5-25 yards)","Long (>25 yards)")) %>%
  mutate(focus=case_when(percent_rank(n)>0.4 ~ TRUE,
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
  theme[["solar"]]() +
  theme(
    axis.line.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    panel.grid.major.x=element_blank()
  ) +
  facet_wrap("key",scales="free") +
  labs(
    title="Completed passes",
    x=element_blank(),
    y=element_blank(),
    caption=caption[[1]]
  ) +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous() +
  scale_colour_manual(values=c("TRUE"=colour[["sfc"]][["black"]],"FALSE"=colour[["sfc"]][["grey"]])) +
  scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["light"]],"FALSE"=colour[["sfc"]][["grey"]]))
# ggsave(here("plots","SFC","PassesCompleted.jpg"))
 
players %>%
  filter_season_team() %>%
  filter(body_parts_left+body_parts_right>0) %>%
  mutate(body_parts_tot=body_parts_left+body_parts_right) %>%
  mutate(body_parts_max=ifelse(body_parts_left>=body_parts_right,body_parts_left,body_parts_right)) %>%
  mutate(body_parts_min=ifelse(body_parts_left<body_parts_right,body_parts_left,body_parts_right)) %>%
  mutate(body_parts_ratio=(body_parts_max/body_parts_tot)) %>%
  mutate(body_parts_main=ifelse(body_parts_left>=body_parts_right,"Left","Right")) %>%
  mutate(body_parts_other=ifelse(body_parts_left<body_parts_right,"Left","Right")) %>%
  mutate(player=fct_reorder(player,desc(body_parts_ratio))) %>%
  ggplot(aes(y=player)) +
  geom_segment(aes(x=0,xend=body_parts_max,y=player,yend=player,colour=body_parts_main),size=4,alpha=0.8) +
  geom_segment(aes(x=0,xend=-body_parts_min,y=player,yend=player,colour=body_parts_other),size=4,alpha=0.8) +
  geom_label(aes(x=0,y=player,label=sprintf("%2.0f%%",100*body_parts_ratio)),size=2,label.padding=unit(0.2, "lines"),label.r = unit(0.08, "lines")) +
  theme[["solar"]]() +
  theme(
    plot.title=element_markdown()
  ) +
  labs(
    title=glue("<b style='color:#265DAB'>Left foot</b> / <b style='color:#CB2027'>Right foot</b> passes"),
    x=element_blank(),
    y=element_blank(),
    caption=caption[[1]]
  ) +
  scale_x_continuous(breaks=seq(-2000,2000,200),labels=abs(seq(-2000,2000,200)),expand=expansion(add=c(20))) +
  scale_colour_manual(values=c("Left"=colour[["medium"]][[1]],"Right"=colour[["medium"]][[8]]))
# ggsave(here("plots","SFC","PassFootedness.jpg"))
 
players %>%
  filter_season_team() %>%
  select(player,sca=sca_sca,sca_passlive=sca_types_passlive,sca_passdead=sca_types_passdead,
         sca_drib=sca_types_drib,sca_sh=sca_types_sh,sca_fld=sca_types_fld) %>%
  filter(sca>0) %>%
  mutate(sca_other=sca_drib+sca_sh+sca_fld) %>%
  make_long_data(levels=c("sca_passlive","sca_passdead","sca_other"),labels=c("Open play pass","Dead ball pass","Dribble/Shot/Fouled")) %>%
  mutate(focus=case_when(
    (key=="Open play pass" & min_rank(desc(n))<=11) ~ TRUE,
    (key=="Dead ball pass" & min_rank(desc(n))<=2) ~ TRUE,
    (key=="Dribble/Shot/Fouled" & min_rank(desc(n))<=5) ~ TRUE,
    TRUE ~ FALSE
  )) %>%
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
  theme[["solar"]]() +
  theme(
    axis.line.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    panel.grid.major.x=element_blank(),
    plot.title=element_text(),
  ) +
  facet_wrap("key",scales="free") +
  labs(
    title="Shot Creating Actions",
    x=element_blank(),
    y=element_blank(),
    caption=glue("Shot Creating Actions are the two actions directly leading to a shot\n",caption[[1]])
  ) +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous() +
  scale_colour_manual(values=c("TRUE"=colour[["sfc"]][["black"]],"FALSE"=colour[["sfc"]][["grey"]])) +
  scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["light"]],"FALSE"=colour[["sfc"]][["grey"]]))
# ggsave(here("plots","SFC","SCA.jpg"))
# 
# matches_long %>%
#   select("Wk":"Opposition","GoalsF":"xGAfbref","Season") %>%
#   filter(Team %in% !!team) %>%
#   filter(Season %in% !!season) %>%
#   filter(!is.na(GoalsF)) %>%
#   mutate(ShortHA=ifelse(HA=="Home","H","A")) %>%
#   mutate(Match=glue::glue("{Opposition} {ShortHA} {GoalsF}-{GoalsA}")) %>%
#   mutate(Match=reorder_within(Match, Date, Season)) %>%
#   ggplot(aes(x=Match)) +
#   geom_point(aes(y=xGFfbref),colour="darkred",alpha=0.8,shape="x") +
#   geom_spline(aes(y=xGFfbref,group=Season),spar=0.6,colour="darkred",linetype="longdash",size=0.7) +
#   geom_point(aes(y=xGAfbref),colour="royalblue",alpha=0.8,shape="x") +
#   geom_spline(aes(y=xGAfbref,group=Season),spar=0.6,colour="royalblue",linetype="longdash",size=0.7) +
#   theme[["solar"]]() +
#   theme(
#     axis.text.x=element_text(size=rel(0.4),angle=60,hjust=1),
#     axis.title.y=element_markdown(size=rel(0.8),colour="black"),
#     axis.text.y=element_text(size=rel(0.8)),
#     plot.title=element_markdown(colour="black"),
#     plot.caption=element_text(colour="black"),
#     strip.text=element_blank()
#   ) +
#   labs(
#     title=glue("Southampton <b style='color:darkred'>attack</b> / <b style='color:royalblue'>defence</b> trend"),
#     x=element_blank(),
#     y=glue("Expected goals <b style='color:darkred'>for</b> / <b style='color:royalblue'>against</b>"),
#     caption=caption[[1]]
#   ) +
#   scale_x_reordered() +
#   scale_y_continuous(limits=c(0,NA),expand=expansion(add=c(0,0.1))) +
#   facet_grid(cols=vars(Season), space="free", scales="free_x")
# ggsave(here("plots","SFC","xGtrend.jpg"))
# 
# matches_long %>%
#   filter(Team %in% !!team) %>%
#   filter(Season %in% !!season) %>%
#   filter(!is.na(GoalsF)) %>%
#   mutate(ShortHA=ifelse(HA=="Home","H","A")) %>%
#   mutate(Match=glue::glue("{Opposition} {ShortHA} {GoalsF}-{GoalsA}")) %>%
#   mutate(Match=reorder_within(Match, desc(Date), Season)) %>%
#   ggplot(aes(y=Match)) +
#   geom_segment(aes(x=0,xend=xGFfbref,y=Match,yend=Match),colour=colour[["sfc"]][["light"]],size=3.5) +
#   geom_segment(aes(x=0,xend=-xGAfbref,y=Match,yend=Match),colour=colour[["medium"]][[1]],size=3.5) +
#   theme[["solar"]]() +
#   theme(
#     plot.title=element_markdown(),
#     axis.text.y=element_text(size=rel(0.8)),
#     strip.text=element_blank()
#   ) +
#   labs(
#     title=glue("<b style='color:#265DAB'>Opposition xG</b> | <b style='color:#D71920'>Southampton xG</b>"),
#     x=element_blank(),
#     y=element_blank(),
#     caption=caption[[1]]
#   ) +
#   scale_x_continuous(breaks=seq(-10,10,1),labels=abs(seq(-10,10,1)),expand=expansion(add=c(0.1,1))) +
#   scale_y_reordered() +
#   facet_grid(rows=vars(Season), space="free", scales="free_y")
# ggsave(here("plots","SFC","MatchxGseg.jpg"))
