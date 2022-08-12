{
  library(tidyverse)
  library(StatsBombR)
  library(ggsoccer)
  library(isotree)
}
{
  FreeCompetitions <- FreeCompetitions()
  # Euro2020 <- FreeCompetitions %>%
  #   filter(competition_id==55)
  Euro2022 <- FreeCompetitions %>%
    filter(competition_id==53)
  
  # Euro2020Matches <- FreeMatches(Euro2020)
  # Euro2020Events <- free_allevents(MatchesDF=Euro2020Matches, Parallel=T) %>% allclean()
  
  Euro2022Matches <- FreeMatches(Euro2022)
  Euro2022Events <- free_allevents(MatchesDF=Euro2022Matches, Parallel=T) %>% allclean()
}
{
  event_types <-
    Euro2022Events %>%
    filter(type.name=="Pass") %>%
    select(type.name,pass.type.name) %>%
    unique() %>%
    print(n=Inf)
  
  passes <-
    Euro2022Events %>%
    # filter pass events
    filter(type.name %in% c("Pass")) %>% 
    # remove set piece pass types identified from event_types above
    filter(!(pass.type.name %in% c("Kick Off","Goal Kick","Corner","Throw-in","Free Kick"))) %>%
    # keep player name and pass location
    select(
      player.name,
      location.x,location.y,
      pass.end_location.x,pass.end_location.y
    ) %>%
    # bin into pitch zones
    mutate(
      # treat the defensive half as one zone
      # split the attacking half into 20 x 20 zones
      across(where(is.numeric) & contains("x"),cut,breaks=c(0,60,80,100,120),.names="{.col}.bin"),
      across(where(is.numeric) & contains("y"),cut,breaks=seq(0,80,20),.names="{.col}.bin")
    ) %>%
    # mutate(location.y.bin=fct_expand(location.y.bin,"[0,80)")) %>%
    mutate(
      # single y axis zone for the defensive half
      location.y.bin=ifelse(location.x.bin=="(0,60]","(0,80]",location.y.bin),
      pass.end_location.y.bin=ifelse(pass.end_location.x.bin=="(0,60]","(0,80]",pass.end_location.y.bin),
    ) %>%
    mutate(across(contains(".bin"),factor)) %>%
    print(n=20)
  
  # count the number of passes a player has made starting and ending in each zone
  passes_count <-
    passes %>%
    group_by(player.name,location.x.bin,location.y.bin,pass.end_location.x.bin,pass.end_location.y.bin,.drop=TRUE) %>%
    summarise(passes=n(),.groups="drop") %>%
    arrange(desc(passes))
  
  # count the total number of passes by each player
  passes_total <-
    passes %>%
    group_by(player.name) %>%
    summarise(passes=n(),.groups="drop") %>%
    arrange(desc(passes))
}
{
  # transition matrix
  # a matrix of passes made by each player
  # x axis - zones
  # y axis - players (1 row per player)
  
  transition_matrix <-
    passes %>%
    group_by(player.name,across(contains("bin"))) %>%
    summarise(passes=n(),.groups="drop") %>%
    pivot_wider(
      names_from = c(location.x.bin,location.y.bin,pass.end_location.x.bin,pass.end_location.y.bin),
      values_from = passes,
      values_fill = 0
    ) %>%
    # divide by total player passes to normalise rows
    inner_join(passes_total) %>%
    mutate(across(!c(player.name,passes),~.x/passes))
}
{
  # create isolation forest using isotree function
  forest <-
    transition_matrix %>%
    select(-player.name) %>%
    isolation.forest(
      data=.,
      ndim=1,
      ntree=500,
      missing_action="fail",
      scoring_metric="density"
    ) %>%
    print
}
{
  # calculate the isolation prediction of each player
  prediction <-
    # join the prediction to the transition matrix containing player names
    bind_cols(
      # predict using isotree function
      predict(forest, transition_matrix) %>%
        tibble("anomaly_score"=.),
      # join to transition matrix
      transition_matrix
    ) %>%
    # keep the player name and prediction
    select(player.name,anomaly_score) %>%
    # join the player passes total
    inner_join(passes_total) %>%
    # only keep players with at least 20 passes
    filter(passes >= 20) %>%
    # order by prediction
    arrange(desc(anomaly_score)) %>%
    print(n=6)
}
{
  # inner join to only keep player data from the top 12 predictions
  inner_join(
    passes,
    prediction %>%
      slice_max(anomaly_score,n=12)
  ) %>%
    # wrap longer names
    mutate(player.name=str_wrap(player.name,width=25)) %>%
    # order by ranking
    mutate(player.name=fct_reorder(player.name,desc(anomaly_score))) %>%
    ggplot() +
    annotate_pitch(dimensions=pitch_statsbomb) +
    geom_segment(aes(x=location.x,xend=pass.end_location.x,y=location.y,yend=pass.end_location.y),size=0.3,arrow=arrow(length=unit(0.1, "cm"))) +
    theme_pitch() +
    theme(
      strip.text=element_text(size=6),
      strip.background=element_blank(),
      plot.caption=element_text(face="italic",size=6)
    ) +
    facet_wrap(vars(player.name),ncol=4) +
    labs(
      title="Most unique Euro 2022 passing profiles",
      caption="Statsbomb 360 data"
    )
  
  ggsave(filename=here::here("plots","test","isolation_forest.png"),dpi=1000)
}
