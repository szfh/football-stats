tidy[["table"]] <- raw[["table"]] %>%
  select(
    -"Top Team Scorer",
    -"Goalkeeper",
    -"Notes",
    -"Attendance",
  )
# attendance as number?

tidy[["squad"]][["standard"]] <- raw[["squad"]][["standard"]] %>%
  rename(
    "Gls"="Gls...6",
    "Ast"="Ast...7",
    "Gls90"="Gls...12",
    "Ast90"="Ast...13",
    "xG"="xG...17",
    "npxG"="npxG...18",
    "xA"="xA...19",
    "xG90"="xG...20",
    "xA90"="xA...21",
    "xG+xA90"="xG+xA",
    "npxG90"="npxG...23",
    "npxG+xA90"="npxG+xA",
  ) %>%
  select(
    -"MP",
    -"xG",
  )

tidy[["squad"]][["passing"]] <- raw[["squad"]][["passing"]] %>%
  rename(
    "TotalCmp"="Cmp...7",
    "TotalAtt"="Att...8",
    "TotalCmp%"="Cmp%...9",
    "ShortCmp"="Cmp...10",
    "ShortAtt"="Att...11",
    "ShortCmp%"="Cmp%...12",
    "MediumCmp"="Cmp...13",
    "MediumAtt"="Att...14",
    "MediumCmp%"="Cmp%...15",
    "LongCmp"="Cmp...16",
    "LongAtt"="Att...17",
    "LongCmp%"="Cmp%...18",
  ) %>% 
  select(
    -"# Pl",
    -"Ast",
    -"xA",
  )

tidy[["squad"]][["shooting"]] <- raw[["squad"]][["shooting"]] %>%
  select(
    -"# Pl",
    -"Gls",
    -"PK",
    -"PKatt",
    -"xG",
    -"npxG",
    -"FK",
  )

tidy[["squad"]][["misc"]] <- raw[["squad"]][["misc"]] %>%
  select(
    -"# Pl",
    -"CrdY",
    -"CrdR",
  )


tidy[["squad"]][["playingtime"]] <- raw[["squad"]][["playingtime"]] %>%
  select(
    -"# Pl",
    -"MP",
    -"Starts",
    -"Min",
  )

# teams <- table %>%
#   left_join(squad_standard_tidy) %>%
#   left_join(squad_passing_tidy) %>%
#   left_join(squad_shooting_tidy) %>%
#   left_join(squad_misc_tidy) %>% 
#   left_join(squad_playingtime_tidy)
