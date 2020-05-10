plot <- list()

plot_scatter <- function(
  data,
  Squad="Southampton",
  Season="2019",
  x,
  y
){
  data %>%
    ggplot() +
    geom_point(aes0(x=x, y=y))
}

plot_scatter(
  data=players,
  x=xG,
  y=xA
)

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


