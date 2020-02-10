
## Understat Data

### Season 2019 plots

![](understat-southampton_files/figure-gfm/plots-1.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-2.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-3.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-4.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-5.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-6.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-7.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-8.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-9.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-10.png)<!-- -->![](understat-southampton_files/figure-gfm/plots-11.png)<!-- -->

### Test plots

    ## # A tibble: 1 x 2
    ##   player_id player_name
    ##       <int> <chr>      
    ## 1      7700 Che Adams

    ## # A tibble: 20 x 2
    ##    team_id team_name              
    ##    <chr>   <chr>                  
    ##  1 71      Aston Villa            
    ##  2 72      Everton                
    ##  3 73      Bournemouth            
    ##  4 74      Southampton            
    ##  5 75      Leicester              
    ##  6 78      Crystal Palace         
    ##  7 79      Norwich                
    ##  8 80      Chelsea                
    ##  9 81      West Ham               
    ## 10 82      Tottenham              
    ## 11 83      Arsenal                
    ## 12 86      Newcastle United       
    ## 13 87      Liverpool              
    ## 14 88      Manchester City        
    ## 15 89      Manchester United      
    ## 16 90      Watford                
    ## 17 92      Burnley                
    ## 18 220     Brighton               
    ## 19 229     Wolverhampton Wanderers
    ## 20 238     Sheffield United

### Shot maps

``` r
get_match_shots(11666) %>%
  mutate(team = ifelse(h_a == "h", h_team, a_team)) %>%
  filter(team == "Southampton") %>%
  ggplot() +
  annotate_pitch(colour="white",fill="chartreuse3",limits=FALSE) +
  geom_point(aes(x=100*X,y= 100*Y),colour=colour_sfc1,size=3) +
  theme_pitch() +
  theme(plot.background = element_rect(fill="chartreuse3"),title=element_text(colour="black")) +
  coord_flip(xlim = c(49, 101),ylim = c(-1, 101)) +
  ggtitle("Brighton v Southampton","Southampton shots")
```

![](understat-southampton_files/figure-gfm/shots-1.png)<!-- -->

### Data

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
