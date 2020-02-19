latest_data <- matches %>%
  filter(!is.na(GoalsHome)&!is.na(GoalsAway)) %>%
  summarise(last(Date)) %>%
  extract2(1)
