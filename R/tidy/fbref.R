raw <- readRDS(file=here("data","raw-fbref.rds"))

if (!exists("tidy",inherits=FALSE)){
  tidy <- list()
}

# player

tidy[["fbref"]][["player"]][["standard"]] <- raw[["fbref"]][["player"]][["standard"]] %>%
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

tidy[["fbref"]][["player"]][["keepers"]] <- raw[["fbref"]][["player"]][["keepers"]] %>%
  rename(
    "GKPKatt"="PKatt",
    "GKPKA"="PKA",
    "GKPKsv"="PKsv",
    "GKPKm"="PKm",
  ) %>%
  select(
    -"Rk",
  )

tidy[["fbref"]][["player"]][["keepersadv"]] <- raw[["fbref"]][["player"]][["keepersadv"]] %>%
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

tidy[["fbref"]][["player"]][["shooting"]] <- raw[["fbref"]][["player"]][["shooting"]] %>%
  rename(
    "ShotFK"="FK",
    "Sh90"="Sh/90",
    "SoT90"="SoT/90",
  ) %>%
  select(
    -"Rk",
  )

tidy[["fbref"]][["player"]][["passing"]] <- raw[["fbref"]][["player"]][["passing"]] %>%
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

tidy[["fbref"]][["player"]][["playingtime"]] <- raw[["fbref"]][["player"]][["playingtime"]] %>%
  rename(
    "Gls+/-"="+/-",
    "Gls+/-90"="+/-90",
    "GlsOn-Off"="On-Off...21",
    "xGOn-Off"="On-Off...26",
  ) %>%
  select(
    -"Rk",
  )

tidy[["fbref"]][["player"]][["misc"]] <- raw[["fbref"]][["player"]][["misc"]] %>%
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

# table

tidy[["fbref"]][["table"]] <- raw[["fbref"]][["table"]] %>%
  select(
    -"Top Team Scorer",
    -"Goalkeeper",
    -"Notes",
  ) %>%
  rename(
    "xGDiff90"="xGDiff/90"
  )

# squad

tidy[["fbref"]][["squad"]][["standard"]] <- raw[["fbref"]][["squad"]][["standard"]] %>%
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

tidy[["fbref"]][["squad"]][["keepers"]] <- raw[["fbref"]][["squad"]][["keepers"]] %>%
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

tidy[["fbref"]][["squad"]][["keepersadv"]] <- raw[["fbref"]][["squad"]][["keepersadv"]] %>%
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

tidy[["fbref"]][["squad"]][["shooting"]] <- raw[["fbref"]][["squad"]][["shooting"]] %>%
  rename(
    "ShotFK"="FK",
    "Sh90"="Sh/90",
    "SoT90"="SoT/90",
  )

tidy[["fbref"]][["squad"]][["passing"]] <- raw[["fbref"]][["squad"]][["passing"]] %>%
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

tidy[["fbref"]][["squad"]][["playingtime"]] <- raw[["fbref"]][["squad"]][["playingtime"]] %>%
  rename(
    "Gls+/-"="+/-",
    "Gls+/-90"="+/-90",
  )

tidy[["fbref"]][["squad"]][["misc"]] <- raw[["fbref"]][["squad"]][["misc"]] %>%
  rename(
    "DribSuc"="Succ",
    "DribAtt"="Att...16",
    "Drib#Pl"="#Pl",
    "DribTkl"="Tkl",
    "DribCont"="Att...21",
    "DribTkl%"="Tkl%",
    "DribPast"="Past",
  )

# matches

tidy[["fbref"]][["matches"]] <- raw[["fbref"]][["matches"]] %>%
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