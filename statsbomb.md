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
