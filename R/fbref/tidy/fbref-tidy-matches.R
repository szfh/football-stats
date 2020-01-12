tidy[["matches"]] <- raw[["matches"]] %>%
  separate("Score",c("GoalsHome","GoalsAway"),sep="[:punct:]") %>%
  rename(
    "xGHome"="xG...6",
    "xGAway"="xG...8",
  ) %>%
  mutate(
    GoalsHome=as.numeric(GoalsHome),
    GoalsAway=as.numeric(GoalsAway),
    Attendance=as.numeric(gsub("\\,","",Attendance)),
      ) %>%
  select(
    -"Match Report",
    -"Notes",
  ) %>%
  drop_na("Wk")

matches <- tidy[["matches"]]