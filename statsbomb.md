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

    ## Joining, by = "id"

    ## Joining, by = "match_id"

    ## Joining, by = "player.id"

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
types <- statsbombdata %>%
  select(type.name) %>%
  unique

teams <- statsbombdata %>%
  select(team.name) %>%
  unique

statsbombdata %>%
  filter
```

    ## # A tibble: 176,488 x 185
    ##    id    index period timestamp minute second possession duration
    ##    <chr> <int>  <dbl> <chr>      <int>  <int>      <int>    <dbl>
    ##  1 9e65~     1      1 00:00:00~      0      0          1    0    
    ##  2 0265~     2      1 00:00:00~      0      0          1    0    
    ##  3 52bf~     3      1 00:00:00~      0      0          1    0    
    ##  4 2207~     4      1 00:00:00~      0      0          1    0    
    ##  5 fca7~     5      1 00:00:01~      0      1          2    1.67 
    ##  6 1509~     6      1 00:00:03~      0      3          2   NA    
    ##  7 4cfc~     7      1 00:00:03~      0      3          2    0.08 
    ##  8 63d6~     8      1 00:00:03~      0      3          2    1.00 
    ##  9 8ee3~     9      1 00:00:04~      0      4          2   NA    
    ## 10 9183~    10      1 00:00:04~      0      4          2    0.164
    ## # ... with 176,478 more rows, and 177 more variables:
    ## #   related_events <list>, location <list>, under_pressure <lgl>,
    ## #   off_camera <lgl>, counterpress <lgl>, out <lgl>, type.id <int>,
    ## #   type.name <chr>, possession_team.id <int>, possession_team.name <chr>,
    ## #   play_pattern.id <int>, play_pattern.name <chr>, team.id <int>,
    ## #   team.name <chr>, tactics.formation <int>, tactics.lineup <list>,
    ## #   player.id <int>, player.name <chr>, position.id <int>,
    ## #   position.name <chr>, pass.length <dbl>, pass.angle <dbl>,
    ## #   pass.end_location <list>, pass.switch <lgl>, pass.cross <lgl>,
    ## #   pass.through_ball <lgl>, pass.assisted_shot_id <chr>,
    ## #   pass.shot_assist <lgl>, pass.aerial_won <lgl>, pass.goal_assist <lgl>,
    ## #   pass.miscommunication <lgl>, pass.no_touch <lgl>,
    ## #   pass.outswinging <lgl>, pass.straight <lgl>, pass.recipient.id <int>,
    ## #   pass.recipient.name <chr>, pass.height.id <int>,
    ## #   pass.height.name <chr>, pass.type.id <int>, pass.type.name <chr>,
    ## #   pass.body_part.id <int>, pass.body_part.name <chr>,
    ## #   pass.outcome.id <int>, pass.outcome.name <chr>,
    ## #   pass.technique.id <int>, pass.technique.name <chr>,
    ## #   carry.end_location <list>, dribble.nutmeg <lgl>,
    ## #   dribble.overrun <lgl>, dribble.outcome.id <int>,
    ## #   dribble.outcome.name <chr>, ball_receipt.outcome.id <int>,
    ## #   ball_receipt.outcome.name <chr>, duel.outcome.id <int>,
    ## #   duel.outcome.name <chr>, duel.type.id <int>, duel.type.name <chr>,
    ## #   interception.outcome.id <int>, interception.outcome.name <chr>,
    ## #   clearance.right_foot <lgl>, clearance.head <lgl>,
    ## #   clearance.left_foot <lgl>, clearance.aerial_won <lgl>,
    ## #   clearance.other <lgl>, clearance.body_part.id <int>,
    ## #   clearance.body_part.name <chr>, shot.statsbomb_xg <dbl>,
    ## #   shot.end_location <list>, shot.key_pass_id <chr>,
    ## #   shot.first_time <lgl>, shot.freeze_frame <list>,
    ## #   shot.aerial_won <lgl>, shot.one_on_one <lgl>, shot.outcome.id <int>,
    ## #   shot.outcome.name <chr>, shot.type.id <int>, shot.type.name <chr>,
    ## #   shot.body_part.id <int>, shot.body_part.name <chr>,
    ## #   shot.technique.id <int>, shot.technique.name <chr>,
    ## #   goalkeeper.end_location <list>, goalkeeper.type.id <int>,
    ## #   goalkeeper.type.name <chr>, goalkeeper.body_part.id <int>,
    ## #   goalkeeper.body_part.name <chr>, goalkeeper.position.id <int>,
    ## #   goalkeeper.position.name <chr>, goalkeeper.technique.id <int>,
    ## #   goalkeeper.technique.name <chr>, goalkeeper.outcome.id <int>,
    ## #   goalkeeper.outcome.name <chr>, ball_recovery.recovery_failure <lgl>,
    ## #   ball_recovery.offensive <lgl>, foul_won.advantage <lgl>,
    ## #   foul_won.defensive <lgl>, substitution.outcome.id <int>,
    ## #   substitution.outcome.name <chr>, substitution.replacement.id <int>,
    ## #   substitution.replacement.name <chr>, ...

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

``` r
jam_shots <- statsbombdata %>%
  filter(team.name=="Jamaica Women's") %>%
  filter(type.name=="Shot")
  # discard(~all(is.na(.x)))
  # View("jamaica shots")

create_Pitch() +
  geom_segment(data=jam_shots,aes(x=location.x,y=location.y,xend=shot.end_location.x,yend=shot.end_location.y),
               lineend="round",size=0.5,arrow=arrow(length=unit(0.15,"cm"))) +
  labs(title="Jamaica shots",subtitle="Women's World Cup 2019") +
  coord_fixed(ratio=105/100)
```

![](statsbomb_files/figure-gfm/plotting%20shots-1.png)<!-- -->

``` r
# prc_pressures <- statsbombdata %>%
#   filter(team.name=="China PR Women's") %>%
#   filter(type.name=="Pressure") %>%
#   discard(~all(is.na(.x))) %>%
#   View("china pressures")
```
