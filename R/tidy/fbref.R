raw <- readRDS(file=here("data","raw-fbref.rds"))

if (!exists("tidy",inherits=FALSE)){
  tidy <- list()
}

# table
tidy[["fbref"]][["table"]] <-
  lapply(
    raw[["fbref"]][["table"]],
    . %>%
      select(
        -"Top Team Scorer",
        -"Goalkeeper",
        -"Notes",
        -"xGDiff/90"
      ) %>%
      rename(
        "GD"="GDiff",
        "xGF"="xG",
        "xGD"="xGDiff"
      )
  )

# matches
tidy[["fbref"]][["matches"]] <-
  lapply(
    raw[["fbref"]][["matches"]],
    .%>%
      separate("Score",c("GoalsHome","GoalsAway"),sep="[:punct:]") %>%
      select(
        -"Match Report",
        -"Notes"
      ) %>%
      rename(
        "xGHome"="xG...6",
        "xGAway"="xG...8"
      ) %>%
      mutate(
        "GoalsHome"=as.numeric(GoalsHome),
        "GoalsAway"=as.numeric(GoalsAway),
        "Attendance"=as.numeric(gsub("\\,","",Attendance))
      ) %>%
      drop_na("Wk")
  )

# player
tidy[["fbref"]][["player"]][["stats"]] <- raw[["fbref"]][["player"]][["stats"]] %>%
  select(
    -"Rk",
    -("Gls...17":"G+A-PK"),
    -("xG...25":"npxG+xA")
  ) %>%
  rename(
    "Gls"="Gls...11",
    "Ast"="Ast...12",
    "xG"="xG...22",
    "npxG"="npxG...23",
    "xA"="xA...24",
  )

tidy[["fbref"]][["player"]][["keepers"]] <- raw[["fbref"]][["player"]][["keepers"]] %>%
  select(
    -"Rk",
    -contains("%"),
    -contains("90"),
  ) %>%
  rename(
    "GKPKatt"="PKatt",
    "GKPKA"="PKA",
    "GKPKsaved"="PKsv",
    "GKPKmissed"="PKm",
  )


tidy[["fbref"]][["player"]][["keepersadv"]] <- raw[["fbref"]][["player"]][["keepersadv"]] %>%
  select(
    -"Rk",
    -"PSxG/SoT",
    -contains("%"),
    -contains("90"),
  ) %>%
  rename(
    "GKGA"="GA",
    "GKPKA"="PKA",
    "GKFKA"="FK",
    "GKCKA"="CK",
    "GKOGA"="OG",
    "PSxGD"="PSxG+/-",
    "GKLCmp"="Cmp",
    "GKLAtt"="Att...19",
    "GKPassAtt"="Att...21",
    "GKPassThr"="Thr",
    "GKPassAvgLen"="AvgLen...24",
    "GKGKPassAtt"="Att...25",
    "GKGKAvgLen"="AvgLen...27",
    "GKCrsAtt"="Att...28",
    "GKCrsStp"="Stp",
    "GKOPA"="#OPA",
    "GKOPAAvgDist"="AvgDist",
  )

tidy[["fbref"]][["player"]][["shooting"]] <- raw[["fbref"]][["player"]][["shooting"]] %>%
  select(
    -"Rk",
    -("G/Sh":"G/SoT"),
    -("npxG/Sh":"np:G-xG"),
    -contains("%"),
    -contains("90"),
  ) %>%
  rename(
    "ShFK"="FK",
  )

tidy[["fbref"]][["player"]][["passing"]] <- raw[["fbref"]][["player"]][["passing"]] %>%
  select(
    -"Rk",
    -"A-xA",
    -contains("%"),
    -contains("90"),
  ) %>%
  rename(
    "TotalCmp"="Cmp...9",
    "TotalAtt"="Att...10",
    "PassTotalDist"="TotDist",
    "PassPrgDist"="PrgDist",
    "ShortCmp"="Cmp...14",
    "ShortAtt"="Att...15",
    "MediumCmp"="Cmp...17",
    "MediumAtt"="Att...18",
    "LongCmp"="Cmp...20",
    "LongAtt"="Att...21",
    "PassProg"="Prog",
  )

tidy[["fbref"]][["player"]][["passingtypes"]] <- raw[["fbref"]][["player"]][["passingtypes"]] %>%
  select(
    -"Rk",
  ) %>%
  rename(
    "PassAtt"="Att",
    "PassLive"="Live",
    "PassDead"="Dead",
    "PassFK"="FK",
    "PassTB"="TB",
    "PassPress"="Press",
    "CrsIn"="In",
    "CrsOut"="Out...19",
    "CrsStr"="Str",
    "PassFK"="FK",
    "PassCmp"="Cmp",
    "PassOffside"="Off",
    "PassOut"="Out...31",
    "PassInt"="Int",
    "PassBlocks"="Blocks",
  )

tidy[["fbref"]][["player"]][["gca"]] <- raw[["fbref"]][["player"]][["gca"]] %>%
  select(
    -"Rk",
    -contains("90"),
  ) %>%
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
  )


tidy[["fbref"]][["player"]][["defense"]] <- raw[["fbref"]][["player"]][["defense"]] %>%
  select(
    -"Rk",
    -contains("%"),
  ) %>%
  rename(
    "Tkl"="Tkl...9",
    "TklDef3rd"="Def 3rd...11",
    "TklMid3rd"="Mid 3rd...12",
    "TklAtt3rd"="Att 3rd...13",
    "DribTkl"="Tkl...14",
    "DribTklAtt"="Att",
    "DribPast"="Past",
    "DribPress"="Press",
    "PressSucc"="Succ",
    "PressDef3rd"="Def 3rd...21",
    "PressMid3rd"="Mid 3rd...22",
    "PressAtt3rd"="Att 3rd...23",
    "Blk"="Blocks",
    "BlkSh"="Sh",
    "BlkSoT"="ShSv",
    "BlkPass"="Pass",
  )

tidy[["fbref"]][["player"]][["possession"]] <- raw[["fbref"]][["player"]][["possession"]] %>%
  select(
    -"Rk",
    -contains("%"),
  ) %>%
  rename(
    "TouchDefPen"="Def Pen",
    "TouchDef3rd"="Def 3rd",
    "TouchMid3rd"="Mid 3rd",
    "TouchAtt3rd"="Att 3rd",
    "TouchAttPen"="Att Pen",
    "TouchLive"="Live",
    "DribSucc"="Succ",
    "DribAtt"="Att",
    "Drib#Pl"="#Pl",
    "DribTotDist"="TotDist",
    "DribProgDist"="PrgDist",
    "PassTarget"="Targ",
    "PassRec"="Rec",
  )

tidy[["fbref"]][["player"]][["playingtime"]] <- raw[["fbref"]][["player"]][["playingtime"]] %>%
  select(
    -"Rk",
    -("+/-":"+/-90"),
    -("xG+/-":"xG+/-90"),
    -contains("%"),
    -contains("90"),
  ) %>%
  rename(
    "MinMP"="Mn/MP",
    "MinStart"="Mn/Start",
    "MinSub"="Mn/Sub",
    "UnusedSub"="unSub",
    "OnGF"="onG",
    "OnGA"="onGA",
    "OnxGF"="onxG",
    "OnxGA"="onxGA",
    "On-OffG"="On-Off...23",
    "On-OffxG"="On-Off...28",
  )

tidy[["fbref"]][["player"]][["misc"]] <- raw[["fbref"]][["player"]][["misc"]] %>%
  select(
    -"Rk",
    -contains("%"),
  ) %>%
  rename(
    "AerialWon"="Won",
    "AerialLost"="Lost",
  )

# squad
tidy[["fbref"]][["squad"]][["stats"]] <- raw[["fbref"]][["squad"]][["stats"]] %>%
  select(
    -("Gls...12":"G+A-PK"),
    -("xG...20":"npxG+xA"),
  ) %>%
  rename(
    "Gls"="Gls...6",
    "Ast"="Ast...7",
    "xG"="xG...17",
    "npxG"="npxG...18",
    "xA"="xA...19",
  )

tidy[["fbref"]][["squad"]][["keepers"]] <- raw[["fbref"]][["squad"]][["keepers"]] %>%
  select(
    -"Starts",
    -"Min",
    -contains("%"),
    -contains("90"),
  ) %>%
  rename(
    "# GK"="# Pl",
    "GKPKatt"="PKatt",
    "GKPKA"="PKA",
    "GKPKsaved"="PKsv",
    "GKPKmissed"="PKm",
  )

tidy[["fbref"]][["squad"]][["keepersadv"]] <- raw[["fbref"]][["squad"]][["keepersadv"]] %>%
  select(
    -"PSxG/SoT",
    -contains("%"),
    -contains("90"),
  ) %>%
  rename(
    "# GK"="# Pl",
    "GKGA"="GA",
    "GKPKA"="PKA",
    "GKFKA"="FK",
    "GKCKA"="CK",
    "GKOGA"="OG",
    "PSxGD"="PSxG+/-",
    "GKLCmp"="Cmp",
    "GKLAtt"="Att...14",
    "GKPassAtt"="Att...16",
    "GKPassThr"="Thr",
    "GKPassAvgLen"="AvgLen...19",
    "GKGKPassAtt"="Att...20",
    "GKGKAvgLen"="AvgLen...22",
    "GKCrsAtt"="Att...23",
    "GKCrsStp"="Stp",
    "GKOPA"="#OPA",
    "GKOPAAvgDist"="AvgDist",
  )

tidy[["fbref"]][["squad"]][["shooting"]] <- raw[["fbref"]][["squad"]][["shooting"]] %>%
  select(
    -("G/Sh":"G/SoT"),
    -("npxG/Sh":"np:G-xG"),
    -contains("%"),
    -contains("90"),
  ) %>%
  rename(
    "ShFK"="FK",
  )

tidy[["fbref"]][["squad"]][["passing"]] <- raw[["fbref"]][["squad"]][["passing"]] %>%
  select(
    -"A-xA",
    -contains("%"),
  ) %>%
  rename(
    "TotalCmp"="Cmp...3",
    "TotalAtt"="Att...4",
    "PassTotalDist"="TotDist",
    "PassPrgDist"="PrgDist",
    "ShortCmp"="Cmp...8",
    "ShortAtt"="Att...9",
    "MediumCmp"="Cmp...11",
    "MediumAtt"="Att...12",
    "LongCmp"="Cmp...14",
    "LongAtt"="Att...15",
    "PassProg"="Prog",
  )

tidy[["fbref"]][["squad"]][["passingtypes"]] <- raw[["fbref"]][["squad"]][["passingtypes"]] %>%
  rename(
    "PassAtt"="Att",
    "PassLive"="Live",
    "PassDead"="Dead",
    "PassFK"="FK",
    "PassTB"="TB",
    "PassPress"="Press",
    "CrsIn"="In",
    "CrsOut"="Out...13",
    "CrsStr"="Str",
    "PassFK"="FK",
    "PassCmp"="Cmp",
    "PassOffside"="Off",
    "PassOut"="Out...25",
    "PassInt"="Int",
    "PassBlocks"="Blocks",
  )

tidy[["fbref"]][["squad"]][["gca"]] <- raw[["fbref"]][["squad"]][["gca"]] %>%
  select(
    -contains("90"),
  ) %>%
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
  select(
    -contains("%"),
  ) %>%
  rename(
    "Tkl"="Tkl...3",
    "TklDef3rd"="Def 3rd...5",
    "TklMid3rd"="Mid 3rd...6",
    "TklAtt3rd"="Att 3rd...7",
    "DribTkl"="Tkl...8",
    "DribTklAtt"="Att",
    "DribPast"="Past",
    "DribPress"="Press",
    "PressSucc"="Succ",
    "PressDef3rd"="Def 3rd...15",
    "PressMid3rd"="Mid 3rd...16",
    "PressAtt3rd"="Att 3rd...17",
    "Blk"="Blocks",
    "BlkSh"="Sh",
    "BlkSoT"="ShSv",
    "BlkPass"="Pass",
  )

tidy[["fbref"]][["squad"]][["possession"]] <- raw[["fbref"]][["squad"]][["possession"]] %>%
  select(
    -contains("%"),
  ) %>%
  rename(
    "TouchDefPen"="Def Pen",
    "TouchDef3rd"="Def 3rd",
    "TouchMid3rd"="Mid 3rd",
    "TouchAtt3rd"="Att 3rd",
    "TouchAttPen"="Att Pen",
    "TouchLive"="Live",
    "DribSucc"="Succ",
    "DribAtt"="Att",
    "Drib#Pl"="#Pl",
    "DribTotDist"="TotDist",
    "DribPrgDist"="PrgDist",
    "PassTarget"="Targ",
    "PassRec"="Rec",
  )

tidy[["fbref"]][["squad"]][["playingtime"]] <- raw[["fbref"]][["squad"]][["playingtime"]] %>%
  select(
    -("+/-":"+/-90"),
    -("xG+/-":"xG+/-90"),
    -("onG":"onxGA"),
    -"Starts",
    -"PPM",
    -contains("%"),
    -contains("90"),
  ) %>%
  rename(
    "MinMP"="Mn/MP",
    "MinStart"="Mn/Start",
    "MinSub"="Mn/Sub",
    "UnusedSubs"="unSub",
  )

tidy[["fbref"]][["squad"]][["misc"]] <- raw[["fbref"]][["squad"]][["misc"]] %>%
  select(
    -contains("%"),
  ) %>%
  rename(
    "AerialWon"="Won",
    "AerialLost"="Lost",
  )
