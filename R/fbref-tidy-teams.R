table <- table_raw %>%
  select(
    -"Top.Team.Scorer",
    -"Goalkeeper",
    -"Notes",
  )

squad_standard_tidy <- squad_standard_raw %>%
  rename(
    "Players"="X..Pl",
    "Gls90"="Gls.1",
    "Ast90"="Ast.1",
    "G+A90"="G.A",
    "G-PK90"="G.PK",
    "G+A-PK90"="G.A.PK",
    "xG90"="xG.1",
    "xA90"="xA.1",
    "xG+xA90"="xG.xA",
    "npxG90"="npxG.1",
    "npxG+xA90"="npxG.xA",
  ) %>%
  select(
    -"MP",
    -"xG",
  )

squad_passing_tidy <- squad_passing_raw %>%
  rename(
    "A-xA"="A.xA",
    "Cmp%"="Cmp.",
    "TotalCmp"="Cmp",
    "TotalAtt"="Att",
    "TotalCmp%"="Cmp.",
    "ShortCmp"="Cmp.1",
    "ShortAtt"="Att.1",
    "ShortCmp%"="Cmp..1",
    "MediumCmp"="Cmp.2",
    "MediumAtt"="Att.2",
    "MediumCmp%"="Cmp..2",
    "LongCmp"="Cmp.3",
    "LongAtt"="Att.3",
    "LongCmp%"="Cmp..3",
    "1/3"="X1.3",
  ) %>% 
  select(
    -"X..Pl",
    -"Ast",
    -"xA",
  )

squad_shooting_tidy <- squad_shooting_raw %>%
  rename(
    "SoT%"="SoT.",
    "Sh/90"="Sh.90",
    "SoT/90"="SoT.90",
    "G/Sh"="G.Sh",
    "G/SoT"="G.SoT",
    "npxG/Sh"="npxG.Sh",
    "G-xG"="G.xG",
    "np:G-xG"="np.G.xG",
  ) %>%
  select(
    -"X..Pl",
    -"Gls",
    -"PK",
    -"PKatt",
    -"xG",
    -"npxG",
    -"FK",
  )

squad_misc_tidy <- squad_misc_raw %>%
  rename(
    "2CrdY"="X2CrdY",
  ) %>%
  select(
    -"X..Pl",
    -"CrdY",
    -"CrdR",
  )


squad_playingtime_tidy <- squad_playingtime_raw %>% 
  rename(
    "Mn/MP"="Mn.MP",
    "Min%"="Min.",
    "Mn/Start"="Mn.Start",
    "Mn/Sub"="Mn.Sub",
    "+/-"="X...",
    "+/-90"="X...90",
    "xG+/-"="xG...",
    "xG+/-90"="xG...90",
  ) %>%
  select(
    -"X..Pl",
    -"MP",
    -"Starts",
    -"Min",
  )

teams <- table %>%
  left_join(squad_standard_tidy) %>%
  left_join(squad_passing_tidy) %>%
  left_join(squad_shooting_tidy) %>%
  left_join(squad_misc_tidy) %>% 
  left_join(squad_playingtime_tidy)
