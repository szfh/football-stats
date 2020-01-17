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

matches_long <- tidy[["matches"]] %>%
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