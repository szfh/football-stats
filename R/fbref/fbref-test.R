source(here::here("R","fbref","library.R"))
source(here("R","fbref","tidy.R"))

data.frame(x=seq(-2,10,1),y=runif(13)) %>%
  ggplot(aes(x,y)) +
  geom_line() +
  theme(axis.title.x=element_text(hjust=0.2))

# pass footnedness two-sided bar
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

# passes by type
# players %>%
#   filter(Squad=="Southampton") %>%
#   filter(!is.na(ShortCmp)&!is.na(MediumCmp)&!is.na(LongCmp)) %>%
#   mutate(Player=fct_reorder(Player,ShortCmp+MediumCmp+LongCmp)) %>%
#   pivot_longer(cols=c(ShortCmp,MediumCmp,LongCmp),names_to="PassType",values_to="Cmp") %>%
#   ggplot(aes(x=Player,y=Cmp,fill=PassType)) +
#   geom_bar(stat="identity") +
#   coord_flip()

# https://ryo-n7.github.io/2019-11-28-visualize-EPL-part-2/
# completed short/medium/long passes faceted
players %>%
  filter(Squad=="Southampton") %>%
  pivot_longer(cols=c(ShortCmp,MediumCmp,LongCmp),names_to="PassType",values_to="Cmp") %>%
  mutate(PassType=factor(PassType,labels=c("Short","Medium","Long"))) %>%
  mutate(focus=ifelse((PassType=="Short"&Cmp>=50)|
                (PassType=="Medium"&Cmp>=200)|
                (PassType=="Long"&Cmp>=4),
                TRUE,FALSE)) %>%
  select(Player,Squad,PassType,Cmp,focus) %>%
  ggplot(aes(x=0,y=Cmp)) +
  geom_point(aes(colour=focus),size=2) +
  geom_text_repel(
    aes(label=ifelse(focus,Player,"")),
    size=3,
    nudge_x=0.3,
    direction="y",
    hjust=0,
    segment.size=0
  ) +
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
  scale_colour_manual(values=c("lightgrey",sfc))

# completed short/medium/long pass % faceted
# players %>%
#   filter(Squad=="Southampton") %>%
#   pivot_longer(cols=c(`ShortCmp%`,`MediumCmp%`,`LongCmp%`),names_to="PassType",values_to="Cmp%") %>%
#   mutate(PassType=factor(PassType,levels=c("ShortCmp%","MediumCmp%","LongCmp%"))) %>%
#   ggplot(aes(x=0,y=`Cmp%`,label=Player,colour=PassType)) +
#   geom_point(size=2) +
#   facet_wrap("PassType") +
#   scale_y_continuous(labels=function(x){paste0(x,"%")})

# matches %>%
#   # mutate(sub="Southampton") %>%
#   filter(Home=="Southampton" | Away=="Southampton") %>%
#   filter(is.na(xGHome)==FALSE) %>%
#   mutate(
#     xGF=ifelse(Home=="Southampton",xGHome,xGAway),
#     xGA=ifelse(Home=="Southampton",xGAway,xGHome)
#   ) %>%
#   ggplot(aes(x=Wk,y=xGF)) +
#   geom_line()

# matches %>%
#   mutate(sub="Southampton") %>%
#   filter(Home==sub | Away==sub) %>%
#   filter(is.na(xGHome)==FALSE) %>%
#   mutate(
#     xGF=ifelse(Home==sub,xGHome,xGAway),
#     xGA=ifelse(Home==sub,xGAway,xGHome),
#     nsub=ifelse(Home==sub,Away,Home),
#     HA=ifelse(Home==sub,"H","A"),
#   ) %>%
#   pivot_longer(
#     cols=c(xGF,xGA),
#     names_to="team",
#     values_to="teamxG",
#   ) %>%
#   ggplot(aes(x=Wk,y=teamxG,colour=team)) +
#   geom_point() +
#   geom_line()

# df <- data.frame(
#   Home=c("Team1","Team3","Team5"),
#   Away=c("Team2","Team4","Team6"),
#   GoalsHome=c(3,0,1),
#   GoalsAway=c(0,2,1)
# )

# df %>%
#   pivot_longer(
#     cols=c(Home,Away),
#     names_to="HomeAway",
#     values_to="Team"
#   ) %>%
#   left_join(
#     df
#   )

# df_long <- data.frame(
#   Team=c("Team1","Team2","Team3","Team4","Team5","Team6"),
#   Opposition=c("Team2","Team1","Team4","Team3","Team6","Team5"),
#   GoalsF=c(3,0,0,2,1,1),
#   GoalsA=c(0,3,2,0,1,1)
# )

# minutes played by position


# goals by nationality
players %>%
  filter(Squad=="Southampton") %>%
  group_by(Nation) %>%
  tally(Gls) %>%
  rename("Gls"="n") %>%
  mutate(Nation=fct_reorder(Nation,Gls)) %>%
  filter(Gls!=0) %>%
  ggplot(aes(x=Nation,y=Gls,fill=Nation)) +
  geom_bar(stat="identity") +
  theme_sfc() +
  labs(title="Southampton goals by country",
       x=element_blank(),
       y=element_blank()) +
  scale_fill_manual(values=col_medium) +
  coord_flip()

# players_new <- players %>%
#   filter(Squad=="Southampton") %>%
#   mutate(Gls=ifelse(Player %in% c("Jan Bednarek","Shane Long"),Gls+1,Gls))

# players_new["Player"=="Jan Bednarek","Gls"]
# players_new[10,"Gls"`:=`100]
# players_new[,Gls = 100]
# players_new[, a:=1]

# class(players_new)
# 
# players_new %>% view

# players %>%
#   filter(Squad=="Southampton") %>%
#   filter(!is.na(`Gls+/-`)) %>%
#   mutate(Player=fct_reorder(Player,`Gls+/-`)) %>%
#   mutate(PlusMinusPos=ifelse(`Gls+/-`>=0,TRUE,FALSE)) %>%
#   ggplot(aes(x=Player,y=`Gls+/-`)) +
#   geom_bar(aes(fill=PlusMinusPos),stat="identity",width=0.8,alpha=0.6) +
#   theme_sfc() +
#   theme(axis.text.x=element_text(hjust=0.5)) +
#   labs(title="On-pitch goal difference",
#        x=element_blank(),
#        y="Goals scored - Goals conceded",
#        caption=caption[[1]]) +
#   scale_fill_manual(values=c(col_gdocs[[2]],col_gdocs[[4]])) +
#   scale_y_reverse(breaks=seq(-100,100,2),expand=expand_scale(add=0.2)) +
#   coord_flip()

# players %>%
#   filter(Squad=="Southampton") %>%
#   filter(!is.na(Min)) %>%
#   mutate(
#     Min=replace(Min,is.na(Min),0),
#     Subs=replace(Subs,is.na(Subs),0),
#     `Mn/Sub`=replace(`Mn/Sub`,is.na(`Mn/Sub`),0),
#     MinSub=Subs*`Mn/Sub`,
#     MinStart=Min-MinSub,
#   ) %>%
#   mutate(
#     Pos1=ifelse(Player %in% c("Kevin Danso","Jannik Vestergaard","Jan Bednarek","Jack Stephens","Maya Yoshida"),"CB",Pos1),
#     Pos1=ifelse(Player %in% c("Ryan Bertrand","Cédric Soares","Yan Valery"),"FB",Pos1),
#     Pos1=ifelse(Player %in% c("James Ward-Prowse","Pierre Højbjerg","Oriol Romeu","William Smallbone"),"DM",Pos1),
#     Pos1=ifelse(Player %in% c("Nathan Redmond","Stuart Armstrong","Sofiane Boufal","Moussa Djenepo"),"AM",Pos1),
#   ) %>%
#   mutate(Possfc=factor(Pos1,levels=c("GK","CB","FB","DM","AM","FW"))) %>%
#   mutate(Player=fct_reorder(Player,Min)) %>%
#   ggplot(aes(x=Min,y=Player)) +
#   geom_segment(aes(y=Player,yend=Player,x=0,xend=MinStart),colour=sfc,size=3.5,alpha=0.8) +
#   geom_segment(aes(y=Player,yend=Player,x=MinStart,xend=Min),colour=sfc,size=3.5,alpha=0.3) +
#   theme_sfc() +
#   labs(title="League minutes",
#        subtitle="(from start / from bench)",
#        x=element_blank(),
#        y=element_blank(),
#        caption=caption[[1]]) +
#   expand_limits(Min=0) +
#   scale_x_continuous(breaks=seq(0,90*38,180),expand=expand_scale(add=c(20,20))) +
#   theme(
#     axis.line=element_blank(),
#     axis.text.x=element_text(size=rel(0.8),hjust=0.5),
#     axis.text.y=element_text(size=rel(0.8)),
#     strip.background=element_blank(),
#     strip.text.y=element_text(colour="black",face="bold",angle=0),
#   ) +
#   facet_grid(Possfc ~ .,scales="free",space="free")

# players %>%
#   filter(Squad=="Southampton") %>%
#   filter(!is.na(`Gls+/-`)) %>%
#   mutate(Player=fct_reorder(Player,`Gls+/-`)) %>%
#   mutate(Pos=ifelse(`Gls+/-`>=0,TRUE,FALSE)) %>%
#   ggplot(aes(x=`Gls+/-`,y=Player,colour=Pos)) +
#   geom_segment(aes(x=0,xend=`Gls+/-`,y=Player,yend=Player),size=4,alpha=0.8) +
#   theme_sfc() +
#   labs(title="On-pitch goal difference",
#        x=element_blank(),
#        y="Goals scored - Goals conceded",
#        caption=caption[[1]]) +
#   scale_colour_manual(values=c(col_medium[8],col_medium[3])) +
#   scale_x_reverse(breaks=seq(-100,100,2),expand=expand_scale(add=0.1))

# match xG plots
matches_long %>%
  filter(Team=="Southampton") %>%
  filter(!is.na(GF)) %>%
  ggplot(aes(y=Wk)) +
  geom_segment(aes(x=0,xend=xGF,y=Wk,yend=Wk),size=3,alpha=0.8,colour=col_medium[[8]]) +
  geom_segment(aes(x=0,xend=-xGA,y=Wk,yend=Wk),size=3,alpha=0.8,colour=col_medium[[1]]) +
  theme_epl() +
  scale_x_continuous(breaks=seq(-10,10,1),labels=abs(seq(-10,10,1)),expand=expand_scale(add=c(0.1)))

squad %>%
  ggplot(aes(x=npxG,y=Sh,colour=Squad)) +
  geom_point() +
  geom_text_repel(aes(label=Squad),size=3) +
  theme_epl() +
  scale_colour_manual(values=palette_epl())

squad %>%
  ggplot(aes(x=PPA,y=`1/3`,colour=Squad)) +
  geom_point() +
  geom_text_repel(aes(label=Squad),size=3) +
  theme_epl() +
  scale_colour_manual(values=palette_epl())
