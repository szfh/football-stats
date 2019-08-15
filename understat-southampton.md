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

``` r
get_team_players_stats("Southampton",2018) %>%
  filter(time>0) %>%
  ggplot(aes(x=factor(reorder(player_name,time)),y=time)) +
  geom_bar(stat="identity",fill="#e61919",width=0.8,alpha=0.6) +
  labs(title="League minutes 2018-19") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
  coord_flip()
```

![](understat-southampton_files/figure-gfm/plots-1.png)<!-- -->

``` r
get_team_players_stats("Southampton",2018) %>%
  filter(xG > 0) %>%
  ggplot(aes(x=reorder(player_name,xG),y=xG)) +
  geom_bar(stat="identity",fill="#e61919",width=0.8,alpha=0.6) +
  labs(title="League xG 2018-19") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
  coord_flip()
```

![](understat-southampton_files/figure-gfm/plots-2.png)<!-- -->

``` r
get_team_players_stats("Southampton",2018) %>%
  filter((xG > 0)|(xA > 0)) %>%
  ggplot(aes(x=xG,y=xA)) +
  geom_point(colour="#e61919") +
  geom_text(aes(label=ifelse((xG>2)|(xA>2),player_name,""),hjust=0,vjust=0),size=3) +
  labs(title="League xG/xA 2018-19",x="xG",y="xA")
```

![](understat-southampton_files/figure-gfm/plots-3.png)<!-- -->
