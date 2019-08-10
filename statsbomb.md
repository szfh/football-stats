StatsBomb Women’s World Cup Data
================
saintsnumbers
2019-08-09

[**Guide**](http://statsbomb.com/wp-content/uploads/2019/07/Using-StatsBomb-Data-In-R-English.pdf)

1.  Shots and goals
2.  Graphing shots on a chart
3.  Player shots per 90
4.  Mapping passes

## Shots and Goals

``` r
statsbombdata %>%
  group_by(team.name) %>%
  summarise(shots=sum(type.name=="Shot",na.rm=TRUE),
            goals=sum(shot.outcome.name=="Goal",na.rm=TRUE))
```

    ## # A tibble: 24 x 3
    ##    team.name         shots goals
    ##    <chr>             <int> <int>
    ##  1 Argentina Women's    18     2
    ##  2 Australia Women's    57     9
    ##  3 Brazil Women's       51     7
    ##  4 Cameroon Women's     43     3
    ##  5 Canada Women's       61     4
    ##  6 Chile Women's        34     1
    ##  7 China PR Women's     40     1
    ##  8 England Women's      89    13
    ##  9 France Women's       91    10
    ## 10 Germany Women's      77    10
    ## # ... with 14 more rows

## Graphing shots on a chart

``` r
statsbombdata %>%
  group_by(team.name) %>%
  summarise(shots=sum(type.name=="Shot",na.rm=TRUE),
            goals=sum(shot.outcome.name=="Goal",na.rm=TRUE)) %>%
  ggplot(aes(x=reorder(team.name,shots),y=shots)) +
  geom_bar(stat="identity",width=.5) +
  labs(x="Team",y="Shots") +
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(expand=c(0,0)) +
  coord_flip()
```

![](statsbomb_files/figure-gfm/data%20to%20chart-1.png)<!-- -->

``` r
statsbombdata %>%
  group_by(team.name) %>%
  summarise(shots=sum(type.name=="Shot",na.rm=TRUE),
            goals=sum(shot.outcome.name=="Goal",na.rm=TRUE)) %>%
  gather(type,value,-team.name) %>%
  ggplot(aes(x=reorder(team.name,value),y=value,fill=type)) +
  geom_bar(stat="identity",position="dodge") +
  labs(x="Team",y="Shots/Goals") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
  scale_y_continuous(expand=c(0,0)) +
  coord_flip()
```

![](statsbomb_files/figure-gfm/data%20to%20chart-2.png)<!-- -->

## Player shots per 90

``` r
player_minutes <- statsbombdata %>%
  get.minutesplayed() %>%
  group_by(player.id) %>%
  summarise(minutes=sum(MinutesPlayed))
```

    ## Joining, by = "id"

    ## Joining, by = "match_id"

``` r
statsbombdata %>%
  group_by(player.name,player.id) %>%
  summarise(shots=sum(type.name=="Shot",na.rm=TRUE)) %>%
  left_join(player_minutes) %>%
  mutate(shotsper90=(shots/minutes)*90) %>%
  arrange(-shotsper90)
```

    ## Joining, by = "player.id"

    ## # A tibble: 442 x 5
    ## # Groups:   player.name [442]
    ##    player.name                player.id shots minutes shotsper90
    ##    <chr>                          <int> <int>   <dbl>      <dbl>
    ##  1 Alice Ogebe                    26155     1    11.8       7.66
    ##  2 Javiera Grez                   26090     4    50.5       7.13
    ##  3 Carli Lloyd                     5097    17   239.        6.41
    ##  4 Therese Ninon Abena            26095     3    44.7       6.04
    ##  5 Mariela Del Carmen Coronel     25600     1    15.3       5.90
    ##  6 Min-Ji Yeo                     25443     9   142.        5.71
    ##  7 Lana Clelland                  10176     1    16.3       5.53
    ##  8 Georgia Stanway                 4643     8   132.        5.43
    ##  9 Fiona Brown                    10174     1    17.2       5.23
    ## 10 Saori Takarada                 25604     1    18.5       4.87
    ## # ... with 432 more rows
