StatsBomb Women’s World Cup Data
================
saintsnumbers
2019-08-09

[**Guide**](http://statsbomb.com/wp-content/uploads/2019/07/Using-StatsBomb-Data-In-R-English.pdf)

## Import data

``` r
statsbombdata <- FreeCompetitions() %>%
  filter(competition_name=="Women's World Cup" & season_name=="2019") %>%
  FreeMatches() %>%
  StatsBombFreeEvents(Parallel=TRUE) %>%
  allclean()
```

1.  Shots and goals
2.  Graphing shots on a chart
3.  Player shots per 90
4.  Plotting passes
5.  Plotting shots
6.  Plotting pressures

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
statsbombdata %>%
  group_by(player.name,player.id) %>%
  summarise(shots=sum(type.name=="Shot",na.rm=TRUE)) %>%
  left_join(statsbombdata %>%
              get.minutesplayed() %>%
              group_by(player.id) %>%
              summarise(minutes=sum(MinutesPlayed))) %>%
  mutate(shotsper90=(shots/minutes)*90) %>%
  arrange(-shots)
```

    ## # A tibble: 442 x 5
    ## # Groups:   player.name [442]
    ##    player.name              player.id shots minutes shotsper90
    ##    <chr>                        <int> <int>   <dbl>      <dbl>
    ##  1 Vivianne Miedema             15623    23    708.       2.92
    ##  2 Alex Morgan                   5085    20    526.       3.43
    ##  3 Samantha Kerr                 4961    20    418.       4.30
    ##  4 Sara Däbritz                 10263    20    487.       3.69
    ##  5 Ellen White                  10180    18    558.       2.90
    ##  6 Carli Lloyd                   5097    17    239.       6.41
    ##  7 Eugénie Le Sommer            10121    17    438.       3.49
    ##  8 Janine Beckie                 4992    17    363.       4.21
    ##  9 Jennifer Hermoso Fuentes     10151    17    391.       3.92
    ## 10 Amandine Henry               10123    16    517.       2.79
    ## # ... with 432 more rows

## Plotting passes

``` r
eng_passes <- statsbombdata %>%
  filter(team.name=="England Women's") %>%
  filter(type.name=="Pass")

create_Pitch() +
  geom_segment(data=eng_passes,aes(x=location.x,y=location.y,xend=pass.end_location.x,yend=pass.end_location.y),
               lineend="round",size=0.5,arrow=arrow(length=unit(0.15,"cm"))) +
  labs(title="England passes",subtitle="Women's World Cup 2019") +
  coord_fixed(ratio=105/100)
```

![](statsbomb_files/figure-gfm/plotting%20passes-1.png)<!-- -->

## Plotting shots

``` r
# statsbombdata %>%
#   filter(team.name=="Jamaica Women's") %>%
#   filter(type.name=="Shot") %>%
#   discard(~all(is.na(.x))) %>%
#   View("jamaica shots")

jam_shots <- statsbombdata %>%
  filter(team.name=="Jamaica Women's") %>%
  filter(type.name=="Shot")

create_Pitch() +
  geom_segment(data=jam_shots,aes(x=location.x,y=location.y,xend=shot.end_location.x,yend=shot.end_location.y),
               lineend="round",size=0.5,arrow=arrow(length=unit(0.15,"cm"))) +
  labs(title="Jamaica shots",subtitle="Women's World Cup 2019") +
  coord_fixed(ratio=105/100)
```

![](statsbomb_files/figure-gfm/plotting%20shots-1.png)<!-- -->

## Plotting pressures

``` r
# statsbombdata %>%
#   filter(team.name=="China PR Women's") %>%
#   filter(type.name=="Pressure") %>%
#   discard(~all(is.na(.x))) %>%
#   View("china pressures")

prc_pressures <- statsbombdata %>%
  filter(team.name=="China PR Women's") %>%
  filter(type.name=="Pressure")

create_Pitch() +
  geom_segment(data=prc_pressures,aes(x=location.x,y=location.y,xend=location.x,yend=location.y),
               lineend="round",size=0.5) +
  labs(title="China PR Pressures",subtitle="Women's World Cup 2019") +
  coord_fixed(ratio=105/100)
```

![](statsbomb_files/figure-gfm/plotting%20pressures-1.png)<!-- -->
