# Southampton player plots

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
    Player %in% c("Ryan Bertrand","Cédric Soares","Yan Valery") ~ "FB",
    Player %in% c("James Ward-Prowse","Pierre Højbjerg","Oriol Romeu","William Smallbone") ~ "DM",
    Player %in% c("Nathan Redmond","Stuart Armstrong","Sofiane Boufal","Moussa Djenepo") ~ "AM",
    TRUE ~ Pos1)) %>%
  mutate(Possfc=factor(Pos1,levels=c("GK","CB","FB","DM","AM","FW"))) %>%
  mutate(Player=fct_reorder(Player,Min)) %>%
  ggplot(aes(x=Min,y=Player)) +
  geom_segment(aes(y=Player,yend=Player,x=0,xend=MinStart),colour=col_sfc[[1]],size=3.5,alpha=0.8) +
  geom_segment(aes(y=Player,yend=Player,x=MinStart,xend=Min),colour=col_sfc[[1]],size=3.5,alpha=0.3) +
  # geom_label(aes(label=Min),colour=col_sfc[[2]],size=2) +
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
  geom_point(aes(fill=focus),shape=21,size=4,alpha=0.8,colour="black") +
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
  filter(!is.na(Sh)|!is.na(KP)) %>%
  mutate(focus=ifelse(Sh>=10|KP>=10,TRUE,FALSE)) %>%
  ggplot(aes(x=Sh,y=KP)) +
  geom_blank(data=data.frame(Sh=0,KP=0)) +
  geom_point(aes(fill=focus),shape=21,size=3,alpha=0.8,colour="black") +
  geom_text_repel(aes(label=ifelse(focus,Player,"")),size=rel(4)) +
  theme_sfc() +
  labs(title="Southampton shots/shot assists",
       x="Shots",
       y="Passes leading to shot",
       caption=caption[[1]]) +
  scale_x_continuous(breaks=seq(0,300,10),expand=expand_scale(add=c(0,2))) +
  scale_y_continuous(breaks=seq(0,300,10),expand=expand_scale(add=c(0,2))) +
  scale_fill_manual(values=c("TRUE"=col_sfc[[1]],"FALSE"=col_sfc[[3]])) +
  coord_fixed()
ggsave(here("plots","SFC","ShotsKP.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
  filter(!is.na(Sh)|!is.na(KP)) %>%
  mutate(Sh90=90*Sh/Min) %>%
  mutate(KP90=90*KP/Min) %>%
  mutate(focus=ifelse(Sh90>=0.5|KP90>=0.5,TRUE,FALSE)) %>%
  ggplot(aes(x=Sh90,y=KP90)) +
  geom_blank(data=data.frame(Sh90=0,KP90=0)) +
  geom_point(aes(fill=focus),shape=21,size=3,alpha=0.8,colour="black") +
  geom_text_repel(aes(label=ifelse(focus,Player,"")),size=rel(4)) +
  theme_sfc() +
  labs(title="Southampton shots/shot assists",
       subtitle="(per 90 mins)",
       x="Shots",
       y="Passes leading to shot",
       caption=caption[[1]]) +
  scale_x_continuous(breaks=seq(0,20,0.5),expand=expand_scale(add=c(0,0.2))) +
  scale_y_continuous(breaks=seq(0,20,0.5),expand=expand_scale(add=c(0,0.2))) +
  scale_fill_manual(values=c("TRUE"=col_sfc[[1]],"FALSE"=col_sfc[[3]])) +
  coord_fixed()
ggsave(here("plots","SFC","ShotsKP90.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
  filter(!is.na(`Gls+/-`)) %>%
  mutate(Player=fct_reorder(Player,`Gls+/-`)) %>%
  mutate(Pos=ifelse(`Gls+/-`>=0,TRUE,FALSE)) %>%
  ggplot(aes(x=`Gls+/-`,y=Player,colour=Pos)) +
  geom_segment(aes(x=0,xend=`Gls+/-`,y=Player,yend=Player),size=4,alpha=0.8) +
  theme_sfc() +
  labs(title="On-pitch goal difference",
       x=element_blank(),
       y="Goals scored - Goals conceded",
       caption=caption[[1]]) +
  scale_colour_manual(values=c("TRUE"=col_medium[[3]],"FALSE"=col_medium[[8]])) +
  scale_x_reverse(breaks=seq(-100,100,2),expand=expand_scale(add=0.1))
ggsave(here("plots","SFC","GD.jpg"))

# players %>%
#   filter(Squad=="Southampton") %>%
#   filter(!is.na(`xG+/-`)) %>%
#   mutate(Player=fct_reorder(Player,onxGA,.desc=TRUE)) %>%
#   mutate(Pos=ifelse(onxGA>=0,TRUE,FALSE)) %>%
#   ggplot(aes(x=onxGA,y=Player,colour=Pos)) +
#   geom_segment(aes(x=0,xend=onxGA,y=Player,yend=Player),size=4,alpha=0.8) +
#   theme_sfc() +
#   labs(title="On-pitch expected goals against",
#        x="xG difference",
#        y=element_blank(),
#        caption="statsbomb/fbref") +
#   scale_colour_manual(values=c("TRUE"=col_medium[[3]],"FALSE"=col_medium[[8]]))
# ggsave(here("plots","SFC","GoalsAgainstxG.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
  filter(!is.na(`xG+/-`)) %>%
  mutate(Player=fct_reorder(Player,`xG+/-`)) %>%
  mutate(Pos=ifelse(`xG+/-`>=0,TRUE,FALSE)) %>%
  ggplot(aes(x=`xG+/-`,y=Player,colour=Pos)) +
  geom_segment(aes(x=0,xend=`xG+/-`,y=Player,yend=Player),size=4,alpha=0.8) +
  theme_sfc() +
  labs(title="On-pitch expected goals difference",
       x="xG difference",
       y=element_blank(),
       caption=caption[[1]]) +
  scale_colour_manual(values=c("TRUE"=col_medium[[3]],"FALSE"=col_medium[[8]])) +
  scale_x_reverse(breaks=seq(-100,100,2),expand=expand_scale(add=0.1))
ggsave(here("plots","SFC","xGD.jpg"))

players %>%
  filter(Squad=="Southampton") %>%
  filter(!is.na(`xG+/-90`)) %>%
  mutate(Player=fct_reorder(Player,`xG+/-90`)) %>%
  mutate(Pos=ifelse(`xG+/-90`>=0,TRUE,FALSE)) %>%
  ggplot(aes(x=`xG+/-90`,y=Player,colour=Pos)) +
  geom_segment(aes(x=0,xend=`xG+/-90`,y=Player,yend=Player),size=4,alpha=0.8) +
  theme_sfc() +
  labs(title="On-pitch expected goals difference",
       subtitle="(per 90 mins)",
       x=element_blank(),
       y="xG difference per 90 minutes",
       caption=caption[[1]]) +
  scale_colour_manual(values=c("TRUE"=col_medium[[3]],"FALSE"=col_medium[[8]])) +
  scale_x_reverse(breaks=seq(-2,2,0.1),expand=expand_scale(add=0.01))
ggsave(here("plots","SFC","xGD90.jpg"))

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

# Premier League player plots

players %>%
  filter(Gls==0) %>%
  filter(Pos1=="FW" | Pos2=="FW") %>%
  filter(Sh>=3&npxG>=0.3) %>%
  # filter(Player!="David McGoldrick") %>%
  ggplot(aes(x=npxG,y=Sh)) +
  geom_blank(data=data.frame(npxG=0,Sh=0)) +
  geom_text_repel(aes(label=Player),size=2) +
  geom_point(aes(colour=Squad),size=3,shape=4) +
  theme_epl() +
  labs(title="Who hasn't scored yet?",
       subtitle="Forwards with 3+ shots, 0 goals",
       x="Expected goals",
       y="Shots",
       caption=caption[[1]]) +
  scale_colour_manual(values=palette_epl()) +
  expand_limits(x=0,y=10) +
  scale_x_continuous(breaks=seq(0,30,0.5),expand=expand_scale(add=c(0,0.2))) +
  scale_y_continuous(breaks=seq(0,50,5),expand=expand_scale(add=c(0,0.5)))
ggsave(here("plots","EPL","NoGoals.jpg"))

# Premier League team plots

squad %>%
  mutate(Squad=fct_reorder(Squad,npxG)) %>%
  ggplot(aes(x=Squad,y=npxG,colour=Squad)) +
  geom_segment(aes(x=0,xend=npxG,y=Squad,yend=Squad),size=4,alpha=0.8) +
  theme_epl() +
  theme(panel.grid.major.y=element_blank()) +
  labs(title="Premier League expected goals",
       x=element_blank(),
       y=element_blank(),
       caption=caption[[1]]) +
  scale_colour_manual(values=palette_epl()) +
  scale_x_continuous(breaks=seq(0,200,5),expand=expand_scale(add=c(0,0.5)))
ggsave(here("plots","EPL","xG.jpg"))

squad %>%
  mutate(Squad=fct_reorder(Squad,xGA,.desc=TRUE)) %>%
  ggplot(aes(x=Squad,y=xGA,colour=Squad)) +
  geom_segment(aes(x=0,xend=xGA,y=Squad,yend=Squad),size=4,alpha=0.8) +
  theme_epl() +
  theme(panel.grid.major.y=element_blank()) +
  labs(title="Premier League expected goals allowed",
       x=element_blank(),
       y=element_blank(),
       caption=caption[[1]]) +
  scale_colour_manual(values=palette_epl()) +
  scale_x_continuous(breaks=seq(0,200,5),expand=expand_scale(add=c(0,0.5)))
ggsave(here("plots","EPL","xGA.jpg"))

squad %>%
  mutate(Squad=fct_reorder(Squad,`np:G-xG`)) %>%
  ggplot(aes(x=Squad,y=`np:G-xG`,colour=Squad)) +
  geom_segment(aes(x=0,xend=`np:G-xG`,y=Squad,yend=Squad),size=4,alpha=0.8) +
  theme_epl() +
  labs(title="Expected goals over/underperformance",
       x=element_blank(),
       y=element_blank(),
       caption=caption[[1]]) +
  scale_colour_manual(values=palette_epl()) +
  scale_x_continuous(breaks=seq(-50,50,2),expand=expand_scale(add=0.1))
ggsave(here("plots","EPL","G-xG.jpg"))

squad %>%
  mutate(`xGA-GA`=xGA-GA) %>%
  mutate(Squad=fct_reorder(Squad,`xGA-GA`)) %>%
  ggplot(aes(x=Squad,y=`xGA-GA`,colour=Squad)) +
  geom_segment(aes(x=0,xend=`xGA-GA`,y=Squad,yend=Squad),size=4,alpha=0.8) +
  theme_epl() +
  labs(title="Expected goals allowed over/underperformance",
       x=element_blank(),
       y=element_blank(),
       caption=caption[[1]]) +
  scale_colour_manual(values=palette_epl()) +
  scale_x_continuous(breaks=seq(-50,50,2),expand=expand_scale(add=0.1))
ggsave(here("plots","EPL","xGA-GA.jpg"))

squad %>%
  mutate(Squad=fct_reorder(Squad,xGDiff)) %>%
  ggplot(aes(x=Squad,y=xGDiff,colour=Squad)) +
  geom_segment(aes(x=0,xend=xGDiff,y=Squad,yend=Squad),size=4,alpha=0.8) +
  # geom_bar(stat="identity",alpha=0.8) +
  theme_epl() +
  labs(title="Expected goal difference",
       x=element_blank(),
       y=element_blank(),
       caption=caption[[1]]) +
  scale_colour_manual(values=palette_epl()) +
  scale_x_continuous(breaks=seq(-50,50,5),expand=expand_scale(add=0.1))
ggsave(here("plots","EPL","xGD.jpg"))

squad %>%
  select(Squad,xG,xGA) %>%
  mutate(xGA=-xGA) %>%
  pivot_longer(cols=c(xG,xGA),names_to="key",values_to="xG") %>%
  ggplot(aes(x=0,y=xG)) +
  geom_text_repel(
    aes(label=Squad),
    size=4,
    nudge_x=0.5,
    direction="y",
    hjust=0,
    segment.size=0
  ) +
  geom_point(aes(fill=Squad),size=4,shape=21,colour="black") +
  theme_epl() +
  theme(
    strip.text=element_text(size=rel(1.2)),
    axis.line.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    panel.grid.major.x=element_blank(),
  ) +
  facet_wrap("key",scales="free") +
  labs(title=element_blank(),
       x=element_blank(),
       y=element_blank(),
       caption=caption[[1]]) +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous(breaks=seq(-100,100,5),labels=abs(seq(-100,100,5)),expand=expand_scale(add=c(1))) +
  scale_fill_manual(values=palette_epl())
ggsave(here("plots","EPL","xGFxGA1.jpg"))

squad %>%
  select(Squad,xG,xGA) %>%
  mutate(xGA=-xGA) %>%
  mutate(focus=case_when(Squad %in% c("Southampton") ~ TRUE,
                         TRUE ~ FALSE)) %>%
  mutate(Squad=case_when((focus == TRUE) ~ Squad,
                         TRUE ~ "Other")) %>%
  pivot_longer(cols=c(xG,xGA),names_to="key",values_to="xG") %>%
  ggplot(aes(x=0,y=xG)) +
  geom_text_repel(
    aes(label=ifelse(focus,Squad,"")),
    size=rel(5),
    nudge_x=0.5,
    direction="y",
    hjust=0.5,
    segment.size=0
  ) +
  geom_point(aes(fill=Squad),size=4,shape=21,colour="black") +
  theme_epl() +
  theme(
    strip.text=element_text(size=rel(1.2)),
    axis.line.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    panel.grid.major.x=element_blank(),
  ) +
  facet_wrap("key",scales="free") +
  labs(title=element_blank(),
       x=element_blank(),
       y=element_blank(),
       caption=caption[[1]]) +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous(breaks=seq(-100,100,5),labels=abs(seq(-100,100,5)),expand=expand_scale(add=c(1))) +
  scale_fill_manual(values=palette_epl())
ggsave(here("plots","EPL","xGFxGA1_focus.jpg"))

squad %>%
  ggplot(aes(x=npxG,y=xGA)) +
  geom_text_repel(aes(label=Squad),size=rel(3)) +
  geom_point(aes(colour=Squad),size=3,shape=4) +
  theme_epl() +
  labs(title="Expected goals",
       x="xG for",
       y="xG against",
       caption=caption[[1]]) +
  scale_x_continuous(breaks=seq(0,100,5),expand=expand_scale(add=c(4,2))) +
  scale_y_continuous(breaks=seq(0,100,5),expand=expand_scale(add=c(4,2))) +
  scale_colour_manual(values=palette_epl()) +
  coord_fixed()
ggsave(here("plots","EPL","xGFxGA2.jpg"))