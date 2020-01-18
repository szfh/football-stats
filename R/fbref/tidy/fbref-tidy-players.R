tidy[["player"]][["standard"]] <- raw[["player"]][["standard"]] %>%
  rename(
    "Gls"="Gls...11",
    "Ast"="Ast...12",
    "Gls90"="Gls...17",
    "Ast90"="Ast...18",
    "G+A90"="G+A",
    "G-PK90"="G-PK",
    "G+A-PK90"="G+A-PK",
    "xG"="xG...22",
    "npxG"="npxG...23",
    "xA"="xA...24",
    "xG90"="xG...25",
    "xA90"="xA...26",
    "xG+xA90"="xG+xA",
    "npxG90"="npxG...28",
    "npxG+xA90"="npxG+xA",
  ) %>%
  select(
    -"Rk",
  )

tidy[["player"]][["keepers"]] <- raw[["player"]][["keepers"]] %>%
  rename(
    "GKPKatt"="PKatt",
    "GKPKA"="PKA",
    "GKPKsv"="PKsv",
    "GKPKm"="PKm",
  ) %>%
  select(
    -"Rk",
  )

tidy[["player"]][["keepersadv"]] <- raw[["player"]][["keepersadv"]] %>%
  rename(
    "GKGA"="GA",
    "GKPKA"="PKA",
    "GKFK"="FK",
    "GKCK"="CK",
    "GKOG"="OG",
    "PSxG+/-90"="/90",
    "GKLCmp"="Cmp",
    "GKLAtt"="Att...17",
    "GKLCmp%"="Cmp%",
    "GKPassAtt"="Att...19",
    "GKThr"="Thr",
    "GKLaunch%"="Launch%...21",
    "GKAvgLen"="AvgLen...22",
    "GKGKPassAtt"="Att...23",
    "GKGKLaunch%"="Launch%...24",
    "GKGKAvgLen"="AvgLen...25",
    "GKCrsAtt"="Att...26",
    "GKCrsStp"="Stp",
    "GKCrsStp%"="Stp%",
    "GKOPA"="#OPA",
    "GKOPA90"="#OPA/90",
    "GKOPAAvgDist"="AvgDist",
  ) %>%
  select(
    -"Rk",
    -"90s",
  )

tidy[["player"]][["shooting"]] <- raw[["player"]][["shooting"]] %>%
  rename(
    "ShotFK"="FK",
    "Sh90"="Sh/90",
    "SoT90"="SoT/90",
  ) %>%
  select(
    -"Rk",
  )

tidy[["player"]][["passing"]] <- raw[["player"]][["passing"]] %>%
  rename(
    "TotalCmp"="Cmp...11",
    "TotalAtt"="Att...12",
    "TotalCmp%"="Cmp%...13",
    "ShortCmp"="Cmp...14",
    "ShortAtt"="Att...15",
    "ShortCmp%"="Cmp%...16",
    "MediumCmp"="Cmp...17",
    "MediumAtt"="Att...18",
    "MediumCmp%"="Cmp%...19",
    "LongCmp"="Cmp...20",
    "LongAtt"="Att...21",
    "LongCmp%"="Cmp%...22",
    "PassFK"="FK",
  ) %>%
  select(
    -"Rk",
  )

tidy[["player"]][["playingtime"]] <- raw[["player"]][["playingtime"]] %>%
  rename(
    "Gls+/-"="+/-",
    "Gls+/-90"="+/-90",
    "GlsOn-Off"="On-Off...21",
    "xGOn-Off"="On-Off...26",
  ) %>%
  select(
    -"Rk",
  )

tidy[["player"]][["misc"]] <- raw[["player"]][["misc"]] %>%
  rename(
    "DribSuc"="Succ",
    "DribAtt"="Att...20",
    "Drib#Pl"="#Pl",
    "DribTkl"="Tkl",
    "DribCont"="Att...25",
    "DribTkl%"="Tkl%",
    "DribPast"="Past",
  ) %>%
  select(
    -"Rk",
  )

players <- reduce(tidy[["player"]],full_join) %>%
  separate("Player",c("Player",NA),sep="\\\\",fill="right") %>%
  separate("Nation",c(NA,"Nation"),sep=" ",fill="right") %>%
  separate("Pos",c("Pos1",NA,"Pos2"),sep=c(2,3),fill="right")
