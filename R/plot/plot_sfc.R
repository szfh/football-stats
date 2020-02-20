players %>%
  filter(Squad=="Southampton") %>%
  filter(!is.na(Min)) %>%
  mutate(
    Min=replace(Min,is.na(Min),0),
    Subs=replace(Subs,is.na(Subs),0),
    `Mn/Sub`=replace(`Mn/Sub`,is.na(`Mn/Sub`),0),
    MinSub=Subs*`Mn/Sub`,
    MinStart=Min-MinSub,
  ) %>%
  mutate(Pos1=case_when(
    Player %in% c("Kevin Danso","Jannik Vestergaard","Jan Bednarek","Jack Stephens","Maya Yoshida") ~ "CB",
    Player %in% c("Ryan Bertrand","Cédric Soares","Yan Valery","Kyle Walker-Peters") ~ "FB",
    Player %in% c("James Ward-Prowse","Pierre Højbjerg","Oriol Romeu","William Smallbone") ~ "DM",
    Player %in% c("Nathan Redmond","Stuart Armstrong","Sofiane Boufal","Moussa Djenepo") ~ "AM",
    TRUE ~ Pos1)) %>%
  mutate(Possfc=factor(Pos1,levels=c("GK","CB","FB","DM","AM","FW"))) %>%
  mutate(Player=fct_reorder(Player,Min)) %>%
  ggplot(aes(x=Min,y=Player)) +
  geom_segment(aes(y=Player,yend=Player,x=0,xend=MinStart),colour=col_sfc[[1]],size=3.5,alpha=0.8) +
  geom_segment(aes(y=Player,yend=Player,x=MinStart,xend=Min),colour=col_sfc[[1]],size=3.5,alpha=0.3) +
  theme_sfc() +
  labs(title="League minutes",
       subtitle="(from start / from bench)",
       x=element_blank(),
       y=element_blank(),
       caption=caption[[1]]) +
  expand_limits(Min=0) +
  scale_x_continuous(breaks=seq(0,90*38,180),expand=expand_scale(add=c(20,20))) +
  theme(
    axis.line=element_blank(),
    axis.text.x=element_text(size=rel(0.8),hjust=0.5),
    axis.text.y=element_text(size=rel(0.8)),
    strip.text.y=element_text(angle=0),
  ) +
  facet_grid(Possfc ~ .,scales="free",space="free")
ggsave(here("plots","SFC","Minutes.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
  filter(!is.na(npxG)|!is.na(xA)) %>%
  mutate(focus=ifelse(npxG>=1|xA>=1,TRUE,FALSE)) %>%
  ggplot(aes(x=npxG,y=xA)) +
  geom_blank(data=data.frame(npxG=0,xA=0)) +
  geom_point(aes(fill=focus),shape=21,size=4,alpha=0.8,colour=col_sfc[[4]]) +
  geom_text_repel(aes(label=ifelse(focus,Player,"")),size=rel(4)) +
  theme_sfc() +
  labs(title="Southampton xG/xA",
       x="Expected goals",
       y="Expected assists",
       caption=caption[[1]]) +
  scale_x_continuous(breaks=seq(0,30,1),expand=expand_scale(add=c(0,0.2))) +
  scale_y_continuous(breaks=seq(0,30,1),expand=expand_scale(add=c(0,0.2))) +
  scale_fill_manual(values=c("TRUE"=col_sfc[[1]],"FALSE"=col_sfc[[3]])) +
  coord_fixed()
ggsave(here("plots","SFC","xGxA.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
  filter(!is.na(xG90)|!is.na(xA90)) %>%
  mutate(focus=ifelse(xG90>=0.07|xA90>=0.07,TRUE,FALSE)) %>%
  ggplot(aes(x=npxG90,y=xA90)) +
  geom_blank(data=data.frame(npxG90=0,xA90=0)) +
  geom_point(aes(fill=focus),shape=21,size=2,alpha=0.8,colour="black") +
  geom_text_repel(aes(label=ifelse(focus,Player,"")),size=rel(3)) +
  theme_sfc() +
  labs(title="Southampton xG/xA",
       subtitle="(per 90 mins)",
       x="Expected goals per 90 minutes",
       y="Expected assists per 90 minutes",
       caption=caption[[1]]) +
  scale_x_continuous(breaks=seq(0,2,0.1),expand=expand_scale(add=c(0,0.02))) +
  scale_y_continuous(breaks=seq(0,2,0.1),expand=expand_scale(add=c(0,0.02))) +
  scale_fill_manual(values=c("TRUE"=col_sfc[[1]],"FALSE"=col_sfc[[3]])) +
  coord_fixed()
ggsave(here("plots","SFC","xGxA90.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
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
    box.padding=0.05,
  ) +
  geom_point(aes(colour=focus,fill=focus),shape=21,size=2) +
  theme_sfc() +
  theme(
    axis.line.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    panel.grid.major.x=element_blank(),
  ) +
  facet_wrap("ShKP",scales="free") +
  labs(title="Shots / Passes",
       x=element_blank(),
       y=element_blank(),
       caption=caption[[1]]) +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous() +
  scale_colour_manual(values=c("TRUE"=col_sfc[[4]],"FALSE"=col_sfc[[3]])) +
  scale_fill_manual(values=c("TRUE"=col_sfc[[1]],"FALSE"=col_sfc[[3]]))
ggsave(here("plots","SFC","ShotsKP.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
  filter(Min>0) %>%
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
    box.padding=0.05,
  ) +
  geom_point(aes(colour=focus,fill=focus),shape=21,size=2) +
  theme_sfc() +
  theme(
    axis.line.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    panel.grid.major.x=element_blank(),
  ) +
  facet_wrap("ShKP90",scales="free") +
  labs(title="Shots / Passes",
       subtitle="(per 90 mins)",
       x=element_blank(),
       y=element_blank(),
       caption=caption[[1]]) +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous() +
  scale_colour_manual(values=c("TRUE"=col_sfc[[4]],"FALSE"=col_sfc[[3]])) +
  scale_fill_manual(values=c("TRUE"=col_sfc[[1]],"FALSE"=col_sfc[[3]]))
ggsave(here("plots","SFC","ShotsKP90.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
  select(Player,"onG":"xGOn-Off") %>%
  pivot_longer(cols=c("Gls+/-","xG+/-"),names_to="PM",values_to="n") %>%
  mutate(PM=factor(PM,levels=c("Gls+/-","xG+/-"),labels=c("Goals +/-","xG +/-"))) %>%
  mutate(PlusMinus=ifelse(n>=0,TRUE,FALSE)) %>%
  ggplot(aes(x=0,y=n)) +
  geom_text_repel(
    aes(label=Player),
    size=rel(3),
    nudge_x=0.4,
    direction="y",
    hjust=0,
    segment.size=0.4,
    box.padding=0.05,
  ) +
  geom_point(aes(colour=PlusMinus,fill=PlusMinus),shape=21,size=3,colour="black") +
  theme_sfc() +
  theme(
    axis.line.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    panel.grid.major.x=element_blank(),
  ) +
  facet_wrap("PM",scales="free") +
  labs(title="On-pitch goal difference",
       x=element_blank(),
       y=element_blank(),
       caption=caption[[1]]) +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous() +
  scale_fill_manual(values=c("TRUE"=col_medium[[3]],"FALSE"=col_medium[[8]]))
ggsave(here("plots","SFC","GD.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
  select(Player,"onG":"xGOn-Off") %>%
  pivot_longer(cols=c("Gls+/-90","xG+/-90"),names_to="PM",values_to="n") %>%
  mutate(PM=factor(PM,levels=c("Gls+/-90","xG+/-90"),labels=c("Goals +/-","xG +/-"))) %>%
  mutate(PlusMinus=ifelse(n>=0,TRUE,FALSE)) %>%
  ggplot(aes(x=0,y=n)) +
  geom_text_repel(
    aes(label=Player),
    size=rel(3),
    nudge_x=0.4,
    direction="y",
    hjust=0,
    segment.size=0.4,
    box.padding=0.05,
  ) +
  geom_point(aes(colour=PlusMinus,fill=PlusMinus),shape=21,size=3,colour="black") +
  theme_sfc() +
  theme(
    axis.line.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    panel.grid.major.x=element_blank(),
  ) +
  facet_wrap("PM",scales="free") +
  labs(title="On-pitch goal difference",
       subtitle="(per 90 mins)",
       x=element_blank(),
       y=element_blank(),
       caption=caption[[1]]) +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous() +
  scale_fill_manual(values=c("TRUE"=col_medium[[3]],"FALSE"=col_medium[[8]]))
ggsave(here("plots","SFC","GD90.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
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
    box.padding=0.05,
  ) +
  geom_point(aes(colour=focus,fill=focus),shape=21,size=2) +
  theme_sfc() +
  theme(
    axis.line.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    panel.grid.major.x=element_blank(),
  ) +
  facet_wrap("PassType",scales="free") +
  labs(title="Completed passes",
       x=element_blank(),
       y=element_blank(),
       caption=caption[[1]]) +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous() +
  scale_colour_manual(values=c("TRUE"=col_sfc[[4]],"FALSE"=col_sfc[[3]])) +
  scale_fill_manual(values=c("TRUE"=col_sfc[[1]],"FALSE"=col_sfc[[3]]))
ggsave(here("plots","SFC","PassesCompleted.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
  filter(!is.na(Left)&!is.na(Right)) %>%
  mutate(Passes=Left+Right) %>%
  mutate(Player=fct_reorder(Player,Passes)) %>%
  mutate(MaxPass=ifelse(Left>Right,Left,Right)) %>%
  mutate(Ratio=(MaxPass/Passes)) %>%
  ggplot(aes(y=Player)) +
  geom_segment(aes(x=0,xend=-Left,y=Player,yend=Player),size=4,alpha=0.8,colour=col_medium[[1]]) +
  geom_segment(aes(x=0,xend=Right,y=Player,yend=Player),size=4,alpha=0.8,colour=col_medium[[8]]) +
  geom_label(aes(x=0,y=Player,label=sprintf("%2.0f%%",100*Ratio)),size=2) +
  theme_sfc() +
  labs(title="L/R footed passes",
       x=element_blank(),
       y=element_blank(),
       caption=caption[[1]]) +
  scale_x_continuous(breaks=seq(-2000,2000,200),labels=abs(seq(-2000,2000,200)),expand=expand_scale(add=c(20)))
ggsave(here("plots","SFC","PassFootedness.jpg"))