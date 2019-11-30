Table <- Table_raw %>%
  select(
    -"Top Team Scorer",
    -"Goalkeeper",
    -"Notes",
  )

Standard_Team_tidy <- Standard_Team_raw %>%
  rename(
    "Gls90"="Gls_1",
    "Ast90"="Ast_1",
    "G+A90"="G+A",
    "G-PK90"="G-PK",
    "G+A-PK90"="G+A-PK",
    "xG90"="xG_1",
    "xA90"="xA_1",
    "xG+xA90"="xG+xA",
    "npxG90"="npxG_1",
    "npxG+xA90"="npxG+xA",
  ) %>%
  select(
    -"MP",
    -"xG",
  )

Passing_Team_tidy <- Passing_Team_raw %>%
  rename(
    "TotalCmp"="Cmp",
    "TotalAtt"="Att",
    "TotalCmp%"="Cmp%",
    "ShortCmp"="Cmp_1",
    "ShortAtt"="Att_1",
    "ShortCmp%"="Cmp%_1",
    "MediumCmp"="Cmp_2",
    "MediumAtt"="Att_2",
    "MediumCmp%"="Cmp%_2",
    "LongCmp"="Cmp_3",
    "LongAtt"="Att_3",
    "LongCmp%"="Cmp%_3",
  ) %>% 
  select(
    -"# Pl",
    -"Ast",
    -"xA",
  )

Shooting_Team_tidy <- Shooting_Team_raw %>%
  select(
    -"xG",
    -"# Pl",
    -"Gls",
    -"PK",
    -"PKatt",
    -"npxG",
    -"FK",
  )

Misc_Team_tidy <- Misc_Team_raw %>%
  select(
    -"# Pl",
    -"CrdY",
    -"CrdR",
  )


PlayingTime_Team_tidy <- PlayingTime_Team_raw %>% 
  select(
    -"MP",
    -"# Pl",
    -"Starts",
    -"Min",
  )

Teams <- Table %>%
  left_join(Standard_Team_tidy) %>%
  left_join(Passing_Team_tidy) %>%
  left_join(Shooting_Team_tidy) %>%
  left_join(Misc_Team_tidy) %>% 
  left_join(PlayingTime_Team_tidy)