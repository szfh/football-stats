# players
players <- reduce(tidy[["fbref"]][["player"]],full_join) %>%
  separate("Player",c("Player",NA),sep="\\\\",fill="right") %>%
  separate("Nation",c(NA,"Nation"),sep=" ",fill="right") %>%
  separate("Pos",c("Pos1",NA,"Pos2"),sep=c(2,3),fill="right") %>%
  select(
    -"Matches",
  )

# squad
squad <- tidy[["fbref"]][["table"]] %>%
  full_join(reduce(tidy[["fbref"]][["squad"]],full_join))

# matches
matches <- tidy[["fbref"]][["matches"]] %>%
  left_join(tidy[["fivethirtyeight"]][["matches"]]) %>%
  rename(
    xGHfbref=xGHome,
    xGAfbref=xGAway,
    xGH538=xg1,
    xGA538=xg2,
  )

matches_long <- matches %>%
  pivot_longer(cols=c(Home,Away),
               names_to="HA",
               values_to="Team") %>%
  left_join(matches) %>%
  mutate(
    Opposition=ifelse(HA=="Home",Away,Home),
    GF=ifelse(HA=="Home",GoalsHome,GoalsAway),
    GA=ifelse(HA=="Home",GoalsAway,GoalsHome),
    xGFfbref=ifelse(HA=="Home",xGHfbref,xGAfbref),
    xGAfbref=ifelse(HA=="Home",xGAfbref,xGHfbref),
    xGF538=ifelse(HA=="Home",xGH538,xGA538),
    xGA538=ifelse(HA=="Home",xGA538,xGH538),
  ) %>%
  # select(
    # -("xGH":"xGA"),
    # -("Home":"Away"),
  # ) %>%
  view

# matches_join <- matches %>%
#   left_join(spi_matches_rn) %>%
#   filter(!is.na(score1)) %>%
#   # select(Home) %>%
#   # unique() %>%
#   view

# matches_long %>%
#   filter(Team=="Southampton") %>%
#   view

# rm(raw,tidy)

players %>% head(20) %>% view
squad %>% head(10) %>% view
