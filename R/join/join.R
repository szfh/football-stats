# players
players <- reduce(tidy[["fbref"]][["player"]],full_join) %>%
  separate("Player",c("Player",NA),sep="\\\\",fill="right") %>%
  separate("Nation",c(NA,"Nation"),sep=" ",fill="right") %>%
  separate("Pos",c("Pos1",NA,"Pos2"),sep=c(2,3),fill="right")

# squad
squad <- tidy[["fbref"]][["table"]] %>%
  full_join(reduce(tidy[["fbref"]][["squad"]],full_join))

# matches
matches <- tidy[["fbref"]][["matches"]] %>%
  left_join(tidy[["fivethirtyeight"]][["matches"]])

matches_long <- matches %>%
  pivot_longer(cols=c(Home,Away),
               names_to="HA",
               values_to="Team") %>%
  left_join(matches) %>%
  mutate(
    Opposition=ifelse(HA=="Home",Away,Home),
    GF=ifelse(HA=="Home",GoalsHome,GoalsAway),
    GA=ifelse(HA=="Home",GoalsAway,GoalsHome),
    xGF=ifelse(HA=="Home",xGHome,xGAway),
    xGA=ifelse(HA=="Home",xGAway,xGHome),
  ) %>%
  select(
    -("xGHome":"xGAway"),
    -("Home":"Away"),
  )

# matches_join <- matches %>%
#   left_join(spi_matches_rn) %>%
#   filter(!is.na(score1)) %>%
#   # select(Home) %>%
#   # unique() %>%
#   view

# rm(raw,tidy)

# matches_long %>%
#   filter(Team=="Southampton") %>%
#   view
