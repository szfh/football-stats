# Premier League player plots

players %>%
  filter(Gls==0) %>%
  select(Player,Squad,Sh,Gls,npxG) %>%
  mutate(focus=case_when(percent_rank(Sh)>0.9 ~ TRUE,
                         percent_rank(npxG)>0.9 ~ TRUE,
                         TRUE ~ FALSE
  )) %>%
  mutate(Squad=ifelse(focus,Squad,"Other")) %>%
  ggplot(aes(x=npxG,y=Sh)) +
  geom_blank(data=data.frame(npxG=0,Sh=0)) +
  geom_point(aes(fill=Squad),size=3,shape=21,colour="black",position=position_jitter(0.005)) +
  geom_text_repel(aes(label=ifelse(focus,Player,"")),size=2) +
  theme_epl() +
  labs(title="Players with no goals",
       x="Expected goals",
       y="Shots",
       caption=caption[[1]]) +
  scale_fill_manual(values=palette_epl()) +
  scale_x_continuous(breaks=seq(0,50,1),expand=expand_scale(add=c(0,0.2))) +
  scale_y_continuous(breaks=seq(0,200,5),expand=expand_scale(add=c(0,2)))
ggsave(here("plots","EPL","NoGoals.jpg"))

# Premier League team plots

squad %>%
  select(Squad,xG,xGA) %>%
  mutate(xGA=-xGA) %>%
  pivot_longer(cols=c(xG,xGA),names_to="key",values_to="xG") %>%
  mutate(key=factor(key,levels=c("xG","xGA"),labels=c("xG For","xG Against"))) %>%
  ggplot(aes(x=0,y=xG)) +
  geom_text_repel(
    aes(label=Squad),
    size=rel(3),
    nudge_x=0.5,
    direction="y",
    hjust=0,
    segment.size=0.4,
    segment.alpha=0.8,
    box.padding=0.05,
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
ggsave(here("plots","EPL","xG1.jpg"))

squad %>%
  ggplot(aes(x=npxG,y=xGA)) +
  geom_text_repel(aes(label=Squad),size=rel(3)) +
  geom_point(aes(fill=Squad),shape=21,size=3) +
  theme_epl() +
  labs(title="Expected goals",
       x="xG for",
       y="xG against",
       caption=caption[[1]]) +
  scale_x_continuous(breaks=seq(0,100,5),expand=expand_scale(add=c(4,2))) +
  scale_y_continuous(breaks=seq(0,100,5),expand=expand_scale(add=c(4,2))) +
  scale_fill_manual(values=palette_epl()) +
  coord_fixed()
ggsave(here("plots","EPL","xG2.jpg"))

squad %>%
  select(Squad,GDiff,xGDiff) %>%
  pivot_longer(cols=c(GDiff,xGDiff),names_to="key",values_to="GD") %>%
  mutate(key=factor(key,levels=c("GDiff","xGDiff"),labels=c("Goal Difference","Expected Goal Difference"))) %>%
  ggplot(aes(x=0,y=GD)) +
  geom_text_repel(
    aes(label=Squad),
    size=rel(3),
    nudge_x=0.5,
    direction="y",
    hjust=0,
    segment.size=0.4,
    segment.alpha=0.8,
    box.padding=0.05,
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
  scale_y_continuous(breaks=seq(-100,100,5),expand=expand_scale(add=c(3))) +
  scale_fill_manual(values=palette_epl())
ggsave(here("plots","EPL","xGD.jpg"))