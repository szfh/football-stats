Southampton FC Understat Scraper
================
saintsnumbers
2019-088-14

## Scraping Southampton FC Understat data

### [Understatr package](https://github.com/ewenme/understatr)

``` r
get_leagues_meta() %>%
  View("Leagues")

get_league_teams_stats("EPL",2018) %>%
  View("EPL 2018")

get_team_players_stats("Southampton",2018) %>%
  View("Southampton 2018")

get_player_matches_stats(843) %>%
  View("JWP matches")

get_player_seasons_stats(843) %>%
  View("JWP seasons")
```

## Plots from 2018

### [ggplot2 specs](https://ggplot2.tidyverse.org/articles/ggplot2-specs.html)

``` r
source <- c("Southampton",2018)
get_team_players_stats(source[[1]],source[[2]]) %>%
  filter(time>0) %>%
  mutate(player_name=factor(player_name,levels=player_name[order(time)])) %>%
  ggplot(aes(x=player_name,y=time)) +
  geom_bar(stat="identity",fill="#e61919",width=0.8,alpha=0.6) +
  labs(title=paste0("League minutes ",as.integer(source[[2]]),"-",as.integer(source[[2]])+1)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
  coord_flip()
```

![](understat-southampton_files/figure-gfm/plots-1.png)<!-- -->

``` r
ggsave(paste0("./plots/","TeamStats_",source[[1]],"_",source[[2]],".jpg"))
```

    ## Saving 7 x 5 in image

``` r
source <- c("Southampton",2018)
get_team_players_stats(source[[1]],source[[2]]) %>%
  filter(xG > 0) %>%
  mutate(player_name=factor(player_name,levels=player_name[order(xG)])) %>%
  ggplot(aes(x=player_name,y=xG)) +
  geom_bar(stat="identity",fill="#e61919",width=0.8,alpha=0.6) +
  labs(title=paste0("League xG ",as.integer(source[[2]]),"-",as.integer(source[[2]])+1)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
  coord_flip()
```

![](understat-southampton_files/figure-gfm/plots-2.png)<!-- -->

``` r
ggsave(paste0("./plots/","PlayerStats_",source[[1]],"_",source[[2]],".jpg"))
```

    ## Saving 7 x 5 in image

``` r
source <- c("Southampton",2018)
get_team_players_stats(source[[1]],source[[2]]) %>%
  filter((xG > 0)|(xA > 0)) %>%
  ggplot(aes(x=xG,y=xA)) +
  geom_point(aes(shape="circle"),colour="#e61919",alpha=0.6) +
  geom_text(aes(label=ifelse(xG+xA>=2,player_name,""),hjust="right",vjust="outward"),size=1.5) +
  labs(title=paste0("League xG/xA ",as.integer(source[[2]]),"-",as.integer(source[[2]])+1),x="xG",y="xA") +
  scale_shape_identity()
```

![](understat-southampton_files/figure-gfm/plots-3.png)<!-- -->

``` r
ggsave(paste0("./plots/","xGxA_",source[[1]],"_",source[[2]],"_Point.jpg"))
```

    ## Saving 7 x 5 in image

``` r
source <- c("Southampton",2018)
get_team_players_stats(source[[1]],source[[2]]) %>%
  filter((xG > 0)|(xA > 0)) %>%
  mutate(player_name=factor(player_name,levels=player_name[order(xG)])) %>%
  gather("xG","xA",key="x",value="xN") %>%
  select("player_name","x","xN") %>%
  mutate(x=factor(x,levels=c("xG","xA"))) %>%
  # View("xGxA")
  ggplot(aes(x=player_name,y=xN,fill=x)) +
  geom_bar(stat="identity",position="dodge") +
  labs(title=paste0("League xG/xA ",as.integer(source[[2]]),"-",as.integer(source[[2]])+1)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.title=element_blank()) +
  coord_flip()
```

![](understat-southampton_files/figure-gfm/plots-4.png)<!-- -->

``` r
ggsave(paste0("./plots/","xGxA_",source[[1]],"_",source[[2]],"_Bar.jpg"))
```

    ## Saving 7 x 5 in image
