players <- reduce(tidy[["fbref"]][["player"]],full_join) %>%
  separate("Player",c("Player",NA),sep="\\\\",fill="right") %>%
  separate("Nation",c(NA,"Nation"),sep=" ",fill="right") %>%
  separate("Pos",c("Pos1",NA,"Pos2"),sep=c(2,3),fill="right") %>%
  select(
    -"Matches",
  )

players %>%
  select(Player,Pos1,Squad,TouchAtt3rd,PresAtt3rd,TklAtt3rd) %>%
  filter(!is.na(TouchAtt3rd),!is.na(PresAtt3rd)|!is.na(TklAtt3rd)) %>% 
  pivot_longer(cols=c(TouchAtt3rd,PresAtt3rd,TklAtt3rd),names_to="Stat",values_to="n") %>%
  mutate(Stat=factor(Stat,levels=c("TouchAtt3rd","PresAtt3rd","TklAtt3rd"),labels=c("Touches","Presses","Tackles"))) %>%
  group_by(Stat,Squad) %>%
  mutate(focus=ifelse(min_rank(desc(n))==1,TRUE,FALSE)) %>%
  ggplot(aes(x=0,y=n)) +
  geom_text_repel(
    aes(label=ifelse(focus,Player,"")),
    size=rel(3),
    nudge_x=0.3,
    direction="y",
    hjust=0,
    segment.size=0.4,
    segment.alpha=0.8,
    box.padding=0.05,
  ) +
  geom_point(aes(fill=Squad,alpha=focus),shape=21,size=2,position=position_jitterdodge(jitter.width=0,jitter.height=0.2,dodge.width=0)) +
  theme_sfc() +
  theme(
    axis.line.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    panel.grid.major.x=element_blank(),
  ) +
  facet_wrap("Stat",scales="free") +
  labs(
    title="2019-20 attacking third actions",
    x=element_blank(),
    y=element_blank(),
    caption=caption[[1]]
  ) +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous() +
  scale_fill_manual(values=palette_epl()) +
  scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.2))
ggsave(here("plots","EPL","Att3rdActions.jpg"))