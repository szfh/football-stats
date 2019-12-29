Matches <- Matches_raw %>%
  rename(
    "xGHome"="xG",
    "xGAway"="xG_1",
  ) %>%
  separate("Score",c("GoalsHome","GoalsAway"),sep="[:punct:]") %>%
  select(
    -"Match Report",
    -"Notes",
  ) %>%
  drop_na()