raw <- readRDS(file=here("data","raw-fbref.rds"))

if (!exists("tidy",inherits=FALSE)){
  tidy <- list()
}

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

# matches

tidy[["fbref"]][["matches"]] <- raw[["fbref"]][["matches"]] %>%
  separate("Score",c("GoalsHome","GoalsAway"),sep="[:punct:]") %>%
  rename(
    "xGHome"="xG...6",
    "xGAway"="xG...8",
  ) %>%
  mutate(
    "GoalsHome"=as.numeric(GoalsHome),
    "GoalsAway"=as.numeric(GoalsAway),
    "Attendance"=as.numeric(gsub("\\,","",Attendance)),
  ) %>%
  select(
    -"Match Report",
    -"Notes",
  ) %>%
  drop_na("Wk")

# player

tidy[["fbref"]][["player"]][["stats"]] <- raw[["fbref"]][["player"]][["stats"]] %>%
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
    "GKLAtt"="Att...19",
    "GKLCmp%"="Cmp%",
    "GKPassAtt"="Att...21",
    "GKThr"="Thr",
    "GKLaunch%"="Launch%...23",
    "GKAvgLen"="AvgLen...24",
    "GKGKPassAtt"="Att...25",
    "GKGKLaunch%"="Launch%...26",
    "GKGKAvgLen"="AvgLen...27",
    "GKCrsAtt"="Att...28",
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
    "TotalCmp"="Cmp...9",
    "TotalAtt"="Att...10",
    "TotalCmp%"="Cmp%...11",
    "ShortCmp"="Cmp...14",
    "ShortAtt"="Att...15",
    "ShortCmp%"="Cmp%...16",
    "MediumCmp"="Cmp...17",
    "MediumAtt"="Att...18",
    "MediumCmp%"="Cmp%...19",
    "LongCmp"="Cmp...20",
    "LongAtt"="Att...21",
    "LongCmp%"="Cmp%...22",
  ) %>%
  select(
    -"Rk",
  )
#
tidy[["fbref"]][["player"]][["passingtypes"]] <- raw[["fbref"]][["player"]][["passingtypes"]] %>%
  rename(
    "PassFK"="FK",
    "CrsIn"="In",
    "CrsOut"="Out...19",
    "CrsStr"="Str",
    "PassCmp"="Cmp",
    "PassOff"="Off",
    "PassOut"="Out...31",
    "PassInt"="Int",
    "PassBlocks"="Blocks",
  ) %>%
  select(
    -"Rk",
  )

tidy[["fbref"]][["player"]][["gca"]] <- raw[["fbref"]][["player"]][["gca"]] %>%
  rename(
    "SCAPassLive"="PassLive...11",
    "SCAPassDead"="PassDead...12",
    "SCADrib"="Drib...13",
    "SCASh"="Sh...14",
    "SCAFld"="Fld...15",
    "GCAPassLive"="PassLive...18",
    "GCAPassDead"="PassDead...19",
    "GCADrib"="Drib...20",
    "GCASh"="Sh...21",
    "GCAFld"="Fld...22",
    "GCAOG"="OG",
  ) %>%
  select(
    -"Rk",
  )

tidy[["fbref"]][["player"]][["defense"]] <- raw[["fbref"]][["player"]][["defense"]] %>%
  rename(
    "PlTkl"="Tkl...9",
    "TklDef3rd"="Def 3rd...11",
    "TklMid3rd"="Mid 3rd...12",
    "TklAtt3rd"="Att 3rd...13",
    "DribTkl"="Tkl...14",
    "DribTklAtt"="Att",
    "DribTkl%"="Tkl%",
    "DribPast"="Past",
    "DPress"="Press",
    "PressSucc"="Succ",
    "PressSucc%"="%",
    "PresDef3rd"="Def 3rd...21",
    "PresMid3rd"="Mid 3rd...22",
    "PresAtt3rd"="Att 3rd...23",
    "ShBlk"="Sh",
    "PassBlk"="Pass",
  ) %>%
  select(
    -"Rk",
  )

tidy[["fbref"]][["player"]][["possession"]] <- raw[["fbref"]][["player"]][["possession"]] %>%
  rename(
    "TouchDef3rd"="Def 3rd",
    "TouchMid3rd"="Mid 3rd",
    "TouchAtt3rd"="Att 3rd",
    "TouchAttPen"="Att Pen",
    "TouchLive"="Live",
    "DribSucc"="Succ",
    "DribAtt"="Att",
    "DribSucc%"="Succ%",
    "Drib#Pl"="#Pl",
  ) %>%
  select(
    -"Rk",
    -("Carries":"PrgDist"),
  )

tidy[["fbref"]][["player"]][["playingtime"]] <- raw[["fbref"]][["player"]][["playingtime"]] %>%
  rename(
    "Gls+/-"="+/-",
    "Gls+/-90"="+/-90",
    "GlsOn-Off"="On-Off...23",
    "xGOn-Off"="On-Off...28",
  ) %>%
  select(
    -"Rk",
  )

tidy[["fbref"]][["player"]][["misc"]] <- raw[["fbref"]][["player"]][["misc"]] %>%
  rename(
    
  ) %>%
  select(
    -"Rk",
  )

# squad

tidy[["fbref"]][["squad"]][["stats"]] <- raw[["fbref"]][["squad"]][["stats"]] %>%
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
  )

tidy[["fbref"]][["squad"]][["shooting"]] <- raw[["fbref"]][["squad"]][["shooting"]] %>%
  rename(
    "ShotFK"="FK",
    "Sh90"="Sh/90",
    "SoT90"="SoT/90",
  )

tidy[["fbref"]][["squad"]][["passing"]] <- raw[["fbref"]][["squad"]][["passing"]] %>%
  rename(
    "TotalCmp"="Cmp...3",
    "TotalAtt"="Att...4",
    "TotalCmp%"="Cmp%...5",
    "ShortCmp"="Cmp...8",
    "ShortAtt"="Att...9",
    "ShortCmp%"="Cmp%...10",
    "MediumCmp"="Cmp...11",
    "MediumAtt"="Att...12",
    "MediumCmp%"="Cmp%...13",
    "LongCmp"="Cmp...14",
    "LongAtt"="Att...15",
    "LongCmp%"="Cmp%...16",
  ) 

tidy[["fbref"]][["squad"]][["passingtypes"]] <- raw[["fbref"]][["squad"]][["passingtypes"]] %>%
  rename(
    "PassFK"="FK",
    "CrsIn"="In",
    "CrsOut"="Out...13",
    "CrsStr"="Str",
    "PassCmp"="Cmp",
    "PassOff"="Off",
    "PassOut"="Out...25",
    "PassInt"="Int",
    "PassBlocks"="Blocks",
  )

tidy[["fbref"]][["squad"]][["gca"]] <- raw[["fbref"]][["squad"]][["gca"]] %>%
  rename(
    "SCAPassLive"="PassLive...5",
    "SCAPassDead"="PassDead...6",
    "SCADrib"="Drib...7",
    "SCASh"="Sh...8",
    "SCAFld"="Fld...9",
    "GCAPassLive"="PassLive...12",
    "GCAPassDead"="PassDead...13",
    "GCADrib"="Drib...14",
    "GCASh"="Sh...15",
    "GCAFld"="Fld...16",
    "GCAOG"="OG",
  )

tidy[["fbref"]][["squad"]][["defense"]] <- raw[["fbref"]][["squad"]][["defense"]] %>%
  rename(
    "PlTkl"="Tkl...3",
    "TklDef3rd"="Def 3rd...5",
    "TklMid3rd"="Mid 3rd...6",
    "TklAtt3rd"="Att 3rd...7",
    "DribTkl"="Tkl...8",
    "DribTklAtt"="Att",
    "DribTkl%"="Tkl%",
    "DribPast"="Past",
    "DPress"="Press",
    "PressSucc"="Succ",
    "PressSucc%"="%",
    "PresDef3rd"="Def 3rd...15",
    "PresMid3rd"="Mid 3rd...16",
    "PresAtt3rd"="Att 3rd...17",
    "ShBlk"="Sh",
    "PassBlk"="Pass",
  )

tidy[["fbref"]][["squad"]][["possession"]] <- raw[["fbref"]][["squad"]][["possession"]] %>%
  rename(
    "TouchDefPen"="Def Pen",
    "TouchDef3rd"="Def 3rd",
    "TouchMid3rd"="Mid 3rd",
    "TouchAtt3rd"="Att 3rd",
    "TouchAttPen"="Att Pen",
    "TouchLive"="Live",
    "DribSucc"="Succ",
    "DribAtt"="Att",
    "DribSucc%"="Succ%",
    "Drib#Pl"="#Pl",
    "DribTotDist"="TotDist",
    "DribPrgDist"="PrgDist",
  )

tidy[["fbref"]][["squad"]][["playingtime"]] <- raw[["fbref"]][["squad"]][["playingtime"]] %>%
  rename(
    "Gls+/-"="+/-",
    "Gls+/-90"="+/-90",
  )

tidy[["fbref"]][["squad"]][["misc"]] <- raw[["fbref"]][["squad"]][["misc"]] %>%
  rename()
