# Premier League player plots
players %>%
  filter_season %>%
  select(player,squad,gls=standard_gls,pk=performance_pk,npxg=expected_npxg) %>%
  mutate(npgls=gls-pk) %>%
  make_long_data(levels=c("npgls","npxg"),labels=c("Goals","Expected Goals")) %>%
  mutate(focus=case_when(min_rank(desc(n))<=10 ~ TRUE,
                         TRUE ~ FALSE)) %>%
  ggplot(aes(x=0,y=n,alpha=focus)) +
  geom_text_repel(
    aes(label=ifelse(focus,player,"")),
    size=rel(3),
    nudge_x=0.3,
    direction="y",
    hjust=0,
    segment.size=0.4,
    box.padding=0.05
  ) +
  geom_point(aes(fill=squad),shape=21,size=4,position=position_jitterdodge(jitter.width=0,jitter.height=0.17,dodge.width=0)) +
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
    title="Expected Goals (penalties excluded)",
    x=element_blank(),
    y=element_blank(),
    caption=caption[[1]]) +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous() +
  scale_fill_manual(values=palette[["epl"]]()) +
  scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.1))
# ggsave(here("plots","EPL","PlayerGlsxG.jpg"))

players %>%
  filter_season %>%
  select(player,squad,npxg=expected_npxg,xa=expected_xa) %>%
  make_long_data(levels=c("npxg","xa"),labels=c("xG","xA")) %>%
  mutate(focus=case_when(min_rank(desc(n))<=20 ~ TRUE,
                         TRUE ~ FALSE)) %>%
  ggplot(aes(x=0,y=n,alpha=focus)) +
  geom_text_repel(
    aes(label=ifelse(focus,player,"")),
    size=rel(3),
    nudge_x=0.3,
    direction="y",
    hjust=0,
    segment.size=0.4,
    box.padding=0.05
  ) +
  geom_point(aes(fill=squad),shape=21,size=4) +
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
    title="Expected Goals (penalties excluded)",
    x=element_blank(),
    y=element_blank(),
    caption=caption[[1]]
  ) +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous() +
  scale_fill_manual(values=palette[["epl"]]()) +
  scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.2))
# ggsave(here("plots","EPL","PlayerxGxA.jpg"))
# 
players %>%
  filter_season %>%
  select(player,squad,short_cmp,medium_cmp,long_cmp) %>%
  make_long_data(levels=c("short_cmp","medium_cmp","long_cmp"),labels=c("Short (<5 yards)","Medium (5-25 yards)","Long (>25 yards)")) %>%
  group_by(key,squad) %>%
  mutate(focus=ifelse(min_rank(desc(n))==1,TRUE,FALSE)) %>%
  ggplot(aes(x=0,y=n)) +
  geom_text_repel(
    aes(label=ifelse(focus,player,"")),
    size=rel(3),
    nudge_x=0.3,
    direction="y",
    hjust=0,
    segment.size=0.4,
    segment.alpha=0.8,
    box.padding=0.05
  ) +
  geom_point(aes(fill=squad,alpha=focus),shape=21,size=2,position=position_jitterdodge(jitter.width=0,jitter.height=0.2,dodge.width=0)) +
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
  scale_fill_manual(values=palette[["epl"]]()) +
  scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.2))
# ggsave(here("plots","EPL","PlayerCompPasses.jpg"))
# 
players %>%
  filter_season %>% 
  select(player,squad,sh=standard_sh,gls=standard_gls,npxg=expected_npxg) %>%
  filter(gls==0) %>%
  mutate(focus=case_when(percent_rank(sh)>0.9 ~ TRUE,
                         percent_rank(npxg)>0.9 ~ TRUE,
                         TRUE ~ FALSE
  )) %>%
  mutate(squad=ifelse(focus,squad,"Other")) %>%
  ggplot(aes(x=npxg,y=sh)) +
  geom_blank(data=data.frame(npxg=0,sh=0)) + # maybe delete?
  geom_point(aes(fill=squad),size=3,shape=21,colour="black",position=position_jitter(0.005)) +
  geom_text_repel(aes(label=ifelse(focus,player,"")),size=2) +
  theme[["solar"]]() +
  labs(
    title="Players with no goals",
    x="Expected goals",
    y="Shots",
    caption=caption[[1]]
  ) +
  scale_fill_manual(values=palette[["epl"]]()) +
  scale_x_continuous(breaks=seq(0,50,1),expand=expansion(add=c(0,0.2))) +
  scale_y_continuous(breaks=seq(0,200,5),expand=expansion(add=c(0,2)))
# ggsave(here("plots","EPL","PlayerNoGoals.jpg"))

# Premier League team plots

# squad %>% # needs to be joined to table?
# filter_season %>%
# select(squad,xg=expected_xg,xga=expected_xga) %>%
# mutate(xga=-xga) %>% glimpse
# make_long_data()
#   pivot_longer(cols=c(xG,xGA),names_to="key",values_to="xG") %>%
#   mutate(key=factor(key,levels=c("xG","xGA"),labels=c("xG For","xG Against"))) %>%
#   ggplot(aes(x=0,y=xG)) +
#   geom_text_repel(
#     aes(label=Squad),
#     size=rel(3),
#     nudge_x=0.5,
#     direction="y",
#     hjust=0,
#     segment.size=0.4,
#     segment.alpha=0.8,
#     box.padding=0.05
#   ) +
#   geom_point(aes(fill=Squad),size=4,shape=21,colour="black") +
#   theme[["solar"]]() +
#   theme(
#     strip.text=element_text(size=rel(1.2)),
#     axis.line.x=element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.text.x=element_blank(),
#     panel.grid.major.x=element_blank()
#   ) +
#   facet_wrap("key",scales="free") +
#   labs(
#     title=element_blank(),
#     x=element_blank(),
#     y=element_blank(),
#     caption=caption[[1]]
#   ) +
#   scale_x_continuous(limit=c(0,1)) +
#   scale_y_continuous(breaks=seq(-100,100,5),labels=abs(seq(-100,100,5)),expand=expansion(add=c(1))) +
#   scale_fill_manual(values=palette[["epl"]]())
# ggsave(here("plots","EPL","xG1.jpg"))
# 
# squad %>%
#   filter_season %>%
#   ggplot(aes(x=npxG,y=xGA)) +
#   geom_text_repel(aes(label=Squad),size=rel(3)) +
#   geom_point(aes(fill=Squad),shape=21,size=3) +
#   theme[["solar"]]() +
#   labs(
#     title="Expected goals",
#     x="xG for",
#     y="xG against",
#     caption=caption[[1]]
#   ) +
#   scale_x_continuous(breaks=seq(0,100,5),expand=expansion(add=c(4,2))) +
#   scale_y_continuous(breaks=seq(0,100,5),expand=expansion(add=c(4,2))) +
#   scale_fill_manual(values=palette[["epl"]]()) +
#   coord_fixed()
# ggsave(here("plots","EPL","xG2.jpg"))
# 
# squad %>%
#   filter_season %>%
#   select(Squad,GD,xGD) %>%
#   pivot_longer(cols=c(GD,xGD),names_to="key",values_to="GD") %>%
#   mutate(key=factor(key,levels=c("GD","xGD"),labels=c("Goal Difference","Expected Goal Difference"))) %>%
#   ggplot(aes(x=0,y=GD)) +
#   geom_text_repel(
#     aes(label=Squad),
#     size=rel(3),
#     nudge_x=0.5,
#     direction="y",
#     hjust=0,
#     segment.size=0.4,
#     segment.alpha=0.8,
#     box.padding=0.05
#   ) +
#   geom_point(aes(fill=Squad),size=4,shape=21,colour="black") +
#   theme[["solar"]]() +
#   theme(
#     strip.text=element_text(size=rel(1.2)),
#     axis.line.x=element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.text.x=element_blank(),
#     panel.grid.major.x=element_blank()
#   ) +
#   facet_wrap("key",scales="free") +
#   labs(
#     title=element_blank(),
#     x=element_blank(),
#     y=element_blank(),
#     caption=caption[[1]]
#   ) +
#   scale_x_continuous(limit=c(0,1)) +
#   scale_y_continuous(breaks=seq(-100,100,5),expand=expansion(add=c(3))) +
#   scale_fill_manual(values=palette[["epl"]]())
# ggsave(here("plots","EPL","xGD.jpg"))
# 
players %>%
  select(player,season,pos,squad,touch=touches_att_3rd,pressure=pressures_att_3rd,tackle=tackles_att_3rd) %>%
  filter_season %>%
  make_long_data(levels=c("touch","pressure","tackle"),labels=c("Touches","Pressures","Tackles")) %>%group_by(key,squad) %>%
  mutate(focus=ifelse(min_rank(desc(n))==1,TRUE,FALSE)) %>%
  ggplot(aes(x=0,y=n)) +
  geom_text_repel(
    aes(label=ifelse(focus,player,"")),
    size=rel(3),
    nudge_x=0.3,
    direction="y",
    hjust=0,
    segment.size=0.4,
    segment.alpha=0.8,
    box.padding=0.05
  ) +
  geom_point(aes(fill=squad,alpha=focus),shape=21,size=2,position=position_jitterdodge(jitter.width=0,jitter.height=0.2,dodge.width=0)) +
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
    title="2019-20 attacking third actions",
    x=element_blank(),
    y=element_blank(),
    caption=caption[[1]]
  ) +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous() +
  scale_fill_manual(values=palette[["epl"]]()) +
  scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.2))
# ggsave(here("plots","EPL","Att3rdActions.jpg"))
