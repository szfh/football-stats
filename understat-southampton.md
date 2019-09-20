
# Understat Data

## Season 2019

![](understat-southampton_files/figure-gfm/plots-1.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-2.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-3.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-4.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-5.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-6.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-7.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-8.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-9.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-10.png)<!-- -->

## Shots

``` r
get_match_shots(11666) %>%
  mutate(team = ifelse(h_a == "h", h_team, a_team)) %>%
  # filter(team == "Southampton") %>%
  ggplot(aes(x=X,y=Y,size=xG,colour=team)) +
  geom_point()
```

![](understat-southampton_files/figure-gfm/shots-1.png)<!-- --> \#\#
Data

``` r
get_match_shots(11666) %>%
  mutate(team = ifelse(h_a == "h", h_team, a_team)) %>%
  group_by(team,situation,result) %>%
  summarise(n = n())
```

    ## # A tibble: 14 x 4
    ## # Groups:   team, situation [6]
    ##    team        situation      result          n
    ##    <chr>       <chr>          <chr>       <int>
    ##  1 Brighton    FromCorner     BlockedShot     2
    ##  2 Brighton    FromCorner     MissedShots     1
    ##  3 Brighton    FromCorner     ShotOnPost      1
    ##  4 Brighton    OpenPlay       BlockedShot     1
    ##  5 Brighton    OpenPlay       MissedShots     3
    ##  6 Brighton    OpenPlay       SavedShot       3
    ##  7 Brighton    SetPiece       MissedShots     1
    ##  8 Southampton DirectFreekick MissedShots     2
    ##  9 Southampton FromCorner     BlockedShot     1
    ## 10 Southampton FromCorner     MissedShots     2
    ## 11 Southampton OpenPlay       BlockedShot     2
    ## 12 Southampton OpenPlay       Goal            2
    ## 13 Southampton OpenPlay       MissedShots     1
    ## 14 Southampton OpenPlay       SavedShot       2
