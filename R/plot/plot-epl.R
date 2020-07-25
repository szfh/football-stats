source(here("R","plot","plot-utils.R"))

plots <- list()

# Premier League player plots
plots$glsxg <-
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
    size=2.5,
    nudge_x=0.3,
    direction="y",
    hjust=0,
    segment.size=0.4,
    box.padding=0.05
  ) +
  geom_point(aes(fill=squad),shape=21,size=3,position=position_jitterdodge(jitter.width=0,jitter.height=0.17,dodge.width=0)) +
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

plots$playerxgxa <-
  players %>%
  filter_season %>%
  select(player,squad,npxg=expected_npxg,xa=expected_xa) %>%
  make_long_data(levels=c("npxg","xa"),labels=c("xG","xA")) %>%
  mutate(focus=case_when(min_rank(desc(n))<=20 ~ TRUE,
                         TRUE ~ FALSE)) %>%
  ggplot(aes(x=0,y=n,alpha=focus)) +
  geom_text_repel(
    aes(label=ifelse(focus,player,"")),
    size=2.5,
    nudge_x=0.3,
    direction="y",
    hjust=0,
    segment.size=0.4,
    box.padding=0.05
  ) +
  geom_point(aes(fill=squad),shape=21,size=3) +
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
  players %>%
  filter_season %>%
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
  geom_text_repel(aes(label=ifelse(focus,player,"")),size=2,position=position_jitter(width=0.05,height=0.2,seed=2)) +
  geom_point(aes(fill=squad),size=2,shape=23,colour="black",position=position_jitter(width=0.05,height=0.2,seed=2)) +
  theme[["solar"]]() +
  labs(
    title="Who hasn't scored yet?",
    x="Expected goals",
    y="Shots"
  ) +
  scale_fill_manual(values=palette[["epl"]]()) +
  scale_x_continuous(breaks=seq(0,50,1),expand=expansion(add=c(0,0.2))) +
  scale_y_continuous(breaks=seq(0,200,5),expand=expansion(add=c(0,2)))

# Premier League team plots

plots$squadxg1 <-
  squad %>%
  filter_season %>%
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
  squad %>%
  filter_season %>%
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
  scale_x_continuous(breaks=seq(0,100,5),expand=expansion(add=c(3,1))) +
  scale_y_reverse(breaks=seq(0,100,5),expand=expansion(add=c(3,1))) +
  scale_fill_manual(values=palette[["epl"]]()) +
  coord_fixed()

plots$squadxgd <-
  squad %>%
  filter_season %>%
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
  players %>%
  select(player,season,squad,touch=touches_att_3rd,pressure=pressures_att_3rd,tackle=tackles_att_3rd) %>%
  filter_season %>%
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

plots_wm <- add_watermark(plots,path=here("images","SB_Regular.png"))
save_plots(plots_wm,path=here("plots","EPL"))
