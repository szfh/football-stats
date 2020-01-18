tidy[["table"]] <- raw[["table"]] %>%
  select(
    -"Top Team Scorer",
    -"Goalkeeper",
    -"Notes",
  ) %>%
  rename(
    "xGDiff90"="xGDiff/90"
  )

tidy[["squad"]][["standard"]] <- raw[["squad"]][["standard"]] %>%
  rename(
    "Gls"="Gls...6",
    "Ast"="Ast...7",
    "Gls90"="Gls...12",
    "Ast90"="Ast...13",
    "G+A90"="G+A",
    "G-PK90"="G-PK",
    "G+A-PK90"="G+A-PK",
    "xG"="xG...17",
    "npxG"="npxG...18",
    "xA"="xA...19",
    "xG90"="xG...20",
    "xA90"="xA...21",
    "xG+xA90"="xG+xA",
    "npxG90"="npxG...23",
    "npxG+xA90"="npxG+xA",
  )

tidy[["squad"]][["keepers"]] <- raw[["squad"]][["keepers"]] %>%
  rename(
    "GK# Pl"="# Pl",
    "GKPKatt"="PKatt",
    "GKPKA"="PKA",
    "GKPKsv"="PKsv",
    "GKPKm"="PKm",
  ) %>%
  select(
    -"Starts",
    -"Min",
  )

tidy[["squad"]][["keepersadv"]] <- raw[["squad"]][["keepersadv"]] %>%
  rename(
    "GK# Pl"="# Pl",
    "GKPKA"="PKA",
    "GKFK"="FK",
    "GKCK"="CK",
    "GKOG"="OG",
    "PSxG+/-90"="/90",
    "GKLCmp"="Cmp",
    "GKLAtt"="Att...14",
    "GKLCmp%"="Cmp%",
    "GKPassAtt"="Att...16",
    "GKThr"="Thr",
    "GKLaunch%"="Launch%...18",
    "GKAvgLen"="AvgLen...19",
    "GKGKPassAtt"="Att...20",
    "GKGKLaunch%"="Launch%...21",
    "GKGKAvgLen"="AvgLen...22",
    "GKCrsAtt"="Att...23",
    "GKCrsStp"="Stp",
    "GKCrsStp%"="Stp%",
    "GKOPA"="#OPA",
    "GKOPA90"="#OPA/90",
    "GKOPAAvgDist"="AvgDist",
  ) %>%
  select(
    -"90s",
  )

tidy[["squad"]][["shooting"]] <- raw[["squad"]][["shooting"]] %>%
  rename(
    "ShotFK"="FK",
    "Sh90"="Sh/90",
    "SoT90"="SoT/90",
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
    "PassFK"="FK",
  )

tidy[["squad"]][["playingtime"]] <- raw[["squad"]][["playingtime"]] %>%
  rename(
    "Gls+/-"="+/-",
    "Gls+/-90"="+/-90",
  )

tidy[["squad"]][["misc"]] <- raw[["squad"]][["misc"]] %>%
  rename(
    "DribSuc"="Succ",
    "DribAtt"="Att...16",
    "Drib#Pl"="#Pl",
    "DribTkl"="Tkl",
    "DribCont"="Att...21",
    "DribTkl%"="Tkl%",
    "DribPast"="Past",
  )

squad <- tidy[["table"]] %>%
  full_join(reduce(tidy[["squad"]],full_join))