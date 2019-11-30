Standard_Player_tidy <- Standard_Player_raw %>%
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
  )

Passing_Player_tidy <- Passing_Player_raw %>%
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
    -"Rk",
    -"Ast",
    -"xA",
  )

Shooting_Player_tidy <- Shooting_Player_raw %>%
  select(
    -"Rk",
    -"Gls",
    -"PK",
    -"PKatt",
    -"xG",
    -"npxG",
    -"90s",
    -"FK",
  )

Misc_Player_tidy <- Misc_Player_raw %>%
  select(
    -"Rk",
    -"CrdY",
    -"CrdR",
    -"90s",
  )

PlayingTime_Player_tidy <- PlayingTime_Player_raw %>%
  rename(
    "xGOn-Off"="On-Off_1"
  ) %>%
  select(
    -"Rk",
    -"MP",
    -"Starts",
    -"Min",
    -"90s",
    -"Matches",
  )

Players <- Standard_Player_tidy %>%
  left_join(Passing_Player_tidy) %>%
  left_join(Shooting_Player_tidy) %>%
  left_join(Misc_Player_tidy) %>%
  left_join(PlayingTime_Player_tidy) %>%
  separate("Player",c("Player",NA),sep="\\\\") %>%
  separate("Nation",c(NA,"Nation"),sep=" ")