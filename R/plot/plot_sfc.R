players %>%
  select(Season:Min,MinMP:UnusedSub) %>%
  filter(Squad=="Southampton") %>%
  filter(Season=="2019") %>%
  filter(!is.na(Min)) %>%
  mutate(
    MinStart=Starts*MinStart,
    MinSub=Subs*MinSub
  ) %>%
  mutate(
    Posnew=case_when(
      Player %in% c("Kevin Danso","Jannik Vestergaard","Jan Bednarek","Jack Stephens","Maya Yoshida") ~ "CB",
      Player %in% c("Ryan Bertrand","Cédric Soares","Yan Valery","Kyle Walker-Peters") ~ "FB",
      Player %in% c("James Ward-Prowse","Pierre Højbjerg","Oriol Romeu","William Smallbone") ~ "DM",
      Player %in% c("Nathan Redmond","Stuart Armstrong","Sofiane Boufal","Moussa Djenepo") ~ "AM",
      TRUE ~ Pos)
  ) %>%
  mutate(Possfc=factor(Posnew,levels=c("GK","CB","FB","DM","AM","FW"))) %>%
  mutate(Player=fct_reorder(Player,Min)) %>%
  ggplot(aes(x=Min,y=Player)) +
  geom_segment(aes(y=Player,yend=Player,x=0,xend=MinStart),colour=colour[["sfc"]][["main"]],size=3.5,alpha=0.8) +
  geom_segment(aes(y=Player,yend=Player,x=MinStart,xend=Min),colour=colour[["sfc"]][["light"]],size=3.5,alpha=0.3) +
  theme[["solar"]]() +
  theme(
    plot.title=element_markdown(),
    axis.line=element_blank(),
    # axis.text.x=element_text(size=rel(0.8),hjust=0.5),
    # axis.text.y=element_text(size=rel(0.8)),
    strip.text.y=element_text(angle=0)
  ) +
  labs(
    title=paste0("League minutes (","<b style='color:#D71920'>from start</b>"," / ","<b style='color:#ED5C5C'>from bench</b>",")"),
    x=element_blank(),
    y=element_blank(),
    caption=caption[[1]]
  ) +
  scale_x_continuous(breaks=seq(0,90*38,180),expand=expansion(add=c(0,20))) +
  facet_grid(Possfc ~ .,scales="free",space="free")
ggsave(here("plots","SFC","Minutes.jpg"))

matches_long %>%
  select("Wk":"Opposition","GoalsF":"xGAfbref","Season") %>%
  filter(Team=="Southampton") %>%
  filter(Season %in% c("2017","2018","2019")) %>%
  filter(!is.na(GoalsF)) %>%
  mutate(ShortHA=ifelse(HA=="Home","H","A")) %>%
  mutate(Match=glue::glue("{Opposition} {ShortHA} {GoalsF}-{GoalsA}")) %>%
  mutate(Match=reorder_within(Match, Date, Season)) %>%
  ggplot(aes(x=Match)) +
  geom_point(aes(y=xGFfbref),colour="darkred",alpha=0.8,shape="x") +
  geom_spline(aes(y=xGFfbref,group=Season),spar=0.6,colour="darkred",linetype="longdash",size=0.7) +
  geom_point(aes(y=xGAfbref),colour="royalblue",alpha=0.8,shape="x") +
  geom_spline(aes(y=xGAfbref,group=Season),spar=0.6,colour="royalblue",linetype="longdash",size=0.7) +
  theme[["solar"]]() +
  theme(
    axis.text.x=element_text(size=rel(0.4),angle=60,hjust=1),
    axis.title.y=element_markdown(size=rel(0.8),colour="black"),
    axis.text.y=element_text(size=rel(0.8)),
    plot.title=element_markdown(colour="black"),
    plot.caption=element_text(colour="black")
  ) +
  labs(
    title=paste0("Southampton ","<b style='color:darkred'>attack</b>"," / ","<b style='color:royalblue'>defence</b>"," trend"),
    x=element_blank(),
    y=paste0("Expected goals ","<b style='color:darkred'>for</b>"," / ","<b style='color:royalblue'>against</b>"),
    caption=caption[[1]]
  ) +
  scale_x_reordered() +
  scale_y_continuous(limits=c(0,NA),expand=expansion(add=c(0,0.1))) +
  facet_grid(. ~ Season, space="free", scales="free_x")
ggsave(here("plots","SFC","xGtrend.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
  filter(Season=="2019") %>%
  filter(!is.na(npxG)|!is.na(xA)) %>%
  mutate(focus=ifelse(npxG>=1|xA>=1,TRUE,FALSE)) %>%
  ggplot(aes(x=npxG,y=xA)) +
  geom_point(aes(fill=focus),shape=21,size=4,alpha=0.8,colour=colour[["sfc"]][["black"]]) +
  geom_text_repel(aes(label=ifelse(focus,Player,"")),size=rel(4)) +
  theme[["solar"]]() +
  labs(title="Southampton xG/xA",
       x="Expected goals",
       y="Expected assists",
       caption=caption[[1]]) +
  scale_x_continuous(limits=c(0,NA),breaks=seq(0,30,1),expand=expansion(add=c(0,0.2))) +
  scale_y_continuous(limits=c(0,NA),breaks=seq(0,30,1),expand=expansion(add=c(0,0.2))) +
  scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["main"]],"FALSE"=colour[["sfc"]][["grey"]])) +
  coord_fixed()
ggsave(here("plots","SFC","xGxA.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
  filter(Season=="2019") %>%
  filter(!is.na(Sh)|!is.na(KP)) %>%
  pivot_longer(cols=c(Sh,KP),names_to="ShKP",values_to="n") %>%
  mutate(ShKP=factor(ShKP,levels=c("Sh","KP"),labels=c("Shot","Pass leading to shot"))) %>%
  group_by(ShKP) %>%
  mutate(focus=case_when(percent_rank(n)>0.4 ~ TRUE,
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
  geom_point(aes(colour=focus,fill=focus),shape=21,size=2) +
  theme[["solar"]]() +
  theme(
    axis.line.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    panel.grid.major.x=element_blank()
  ) +
  facet_wrap("ShKP",scales="free") +
  labs(
    title="Shots / Passes",
    x=element_blank(),
    y=element_blank(),
    caption=caption[[1]]) +
  scale_x_continuous(limit=c(0,1)
  ) +
  scale_y_continuous() +
  scale_colour_manual(values=c("TRUE"=colour[["sfc"]][["black"]],"FALSE"=colour[["sfc"]][["grey"]])) +
  scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["light"]],"FALSE"=colour[["sfc"]][["grey"]]))
ggsave(here("plots","SFC","ShotsKP.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
  filter(Season=="2019") %>%
  filter(!is.na(Sh)|!is.na(KP)) %>%
  mutate(Sh90=90*Sh/Min) %>%
  mutate(KP90=90*KP/Min) %>%
  pivot_longer(cols=c(Sh90,KP90),names_to="ShKP90",values_to="n") %>%
  mutate(ShKP90=factor(ShKP90,levels=c("Sh90","KP90"),labels=c("Shot","Pass leading to shot"))) %>%
  group_by(ShKP90) %>%
  mutate(focus=case_when(percent_rank(n)>0.4 ~ TRUE,
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
  geom_point(aes(colour=focus,fill=focus),shape=21,size=2) +
  theme[["solar"]]() +
  theme(
    axis.line.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    panel.grid.major.x=element_blank()
  ) +
  facet_wrap("ShKP90",scales="free") +
  labs(
    title="Shots / Passes",
    subtitle="(per 90 mins)",
    x=element_blank(),
    y=element_blank(),
    caption=caption[[1]]
  ) +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous() +
  scale_colour_manual(values=c("TRUE"=colour[["sfc"]][["black"]],"FALSE"=colour[["sfc"]][["grey"]])) +
  scale_fill_manual(values=c("TRUE"=colour[["sfc"]][["light"]],"FALSE"=colour[["sfc"]][["grey"]]))
ggsave(here("plots","SFC","ShotsKP90.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
  filter(Season=="2019") %>%
  filter(!is.na(`On-OffG`)|!is.na(`On-OffxG`)) %>%
  # select(Player,"On-OffG":"xGOn-Off") %>%
  pivot_longer(cols=c("On-OffG","On-OffxG"),names_to="PM",values_to="n") %>%
  mutate(PM=factor(PM,levels=c("On-OffG","On-OffxG"),labels=c("Goals +/-","xG +/-"))) %>%
  mutate(PlusMinus=ifelse(n>=0,TRUE,FALSE)) %>%
  ggplot(aes(x=0,y=n)) +
  geom_text_repel(
    aes(label=Player),
    size=rel(3),
    nudge_x=0.4,
    direction="y",
    hjust=0,
    segment.size=0.4,
    box.padding=0.05
  ) +
  geom_point(aes(colour=PlusMinus,fill=PlusMinus),shape=21,size=3,colour="black") +
  theme[["solar"]]() +
  theme(
    axis.line.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    panel.grid.major.x=element_blank()
  ) +
  facet_wrap("PM",scales="free") +
  labs(
    title="On-pitch goal difference",
    x=element_blank(),
    y=element_blank(),
    caption=caption[[1]]
  ) +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous() +
  scale_fill_manual(values=c("TRUE"=colour[["medium"]][[3]],"FALSE"=colour[["medium"]][[8]]))
ggsave(here("plots","SFC","GD.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
  filter(Season=="2019") %>%
  filter(!is.na(ShortCmp)|!is.na(MediumCmp)|!is.na(LongCmp)) %>%
  pivot_longer(cols=c(ShortCmp,MediumCmp,LongCmp),names_to="PassType",values_to="Cmp") %>%
  mutate(PassType=factor(PassType,levels=c("ShortCmp","MediumCmp","LongCmp"),labels=c("Short (<5 yards)","Medium (5-25 yards)","Long (>25 yards)"))) %>%
  group_by(PassType) %>%
  mutate(focus=case_when(percent_rank(Cmp)>0.4 ~ TRUE,
                         TRUE ~ FALSE)) %>%
  select(Player,Squad,PassType,Cmp,focus) %>%
  ggplot(aes(x=0,y=Cmp)) +
  geom_text_repel(
    aes(label=ifelse(focus,Player,"")),
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
  facet_wrap("PassType",scales="free") +
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
ggsave(here("plots","SFC","PassesCompleted.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
  filter(Season=="2019") %>%
  filter(!is.na(Left)|!is.na(Right)) %>%
  mutate(Passes=Left+Right) %>%
  mutate(Player=fct_reorder(Player,Passes)) %>%
  mutate(MaxPass=ifelse(Left>Right,Left,Right)) %>%
  mutate(Ratio=(MaxPass/Passes)) %>%
  ggplot(aes(y=Player)) +
  geom_segment(aes(x=0,xend=-Left,y=Player,yend=Player),size=4,alpha=0.8,colour=colour[["medium"]][[1]]) +
  geom_segment(aes(x=0,xend=Right,y=Player,yend=Player),size=4,alpha=0.8,colour=colour[["medium"]][[8]]) +
  geom_label(aes(x=0,y=Player,label=sprintf("%2.0f%%",100*Ratio)),size=2) +
  theme[["solar"]]() +
  labs(
    title="L/R footed passes",
    x=element_blank(),
    y=element_blank(),
    caption=caption[[1]]
  ) +
  scale_x_continuous(breaks=seq(-2000,2000,200),labels=abs(seq(-2000,2000,200)),expand=expansion(add=c(20)))
ggsave(here("plots","SFC","PassFootedness.jpg"))

matches_long %>%
  filter(Team=="Southampton") %>%
  filter(Season=="2019") %>%
  filter(!is.na(GoalsHome)) %>%
  mutate(Match=factor(Wk,labels=paste0(Opposition," ",ifelse(HA=="Home","H","A")," ",GoalsF,"-",GoalsA))) %>%
  mutate(Match=fct_rev(Match)) %>%
  ggplot(aes(y=Match)) +
  geom_segment(aes(x=0,xend=xGFfbref,y=Match,yend=Match),colour=colour[["sfc"]][["light"]],size=3.5,alpha=0.8) +
  geom_segment(aes(x=0,xend=-xGAfbref,y=Match,yend=Match),colour=colour[["medium"]][[1]],size=3.5,alpha=0.8) +
  theme[["solar"]]() +
  theme(
    axis.text.y=element_text(size=rel(0.8))
  ) +
  labs(
    title="              Opposition xG | Southampton xG",
    x=element_blank(),
    y=element_blank(),
    caption=caption[[1]]
  ) +
  scale_x_continuous(breaks=seq(-10,10,1),labels=abs(seq(-10,10,1)),expand=expansion(add=c(0.1)))
ggsave(here("plots","SFC","MatchxGseg.jpg"))
