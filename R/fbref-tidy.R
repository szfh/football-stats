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
  )

Passing_Player_tidy <- Passing_Player_raw %>%
  rename("TotalCmp"="Cmp",
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
  )

Passing_Team_tidy <- Passing_Team_raw %>%
  rename("TotalCmp"="Cmp",
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
  )

Shooting_Player_tidy <- Shooting_Player_raw
Shooting_Team_tidy <- Shooting_Team_raw

Misc_Player_tidy <- Misc_Player_raw
Misc_Team_tidy <- Misc_Team_raw

PlayingTime_Player_tidy <- PlayingTime_Player_raw
PlayingTime_Team_tidy <- PlayingTime_Team_raw

Players <- Standard_Player %>%
  full_join(Passing_Player) %>%
  full_join(Shooting_Player) %>%
  full_join(Misc_Player) %>%
  full_join(PlayingTime_Player)

Teams <- Standard_Team %>%
  full_join(Passing_Team) %>%
  full_join(Shooting_Team) %>%
  full_join(Misc_Team) %>%
  full_join(PlayingTime_Team)

rm(list=ls(pattern="_raw"))
rm(list=ls(pattern="_tidy"))

# EPLMatches <- EPLMatches_raw %>%
#   separate("Score",c("GH","GA"),sep="[:punct:]") %>%
#   rename("xGH"="xG") %>%
#   rename("xGA"="xG_1") %>%
#   filter(!is.na(Wk)) %>%
#   select(-c("Match Report","Notes"))
# 
# EPLPlayerPassing <- EPLPlayerPassing_raw %>%
#   separate("Player",c("Player",NA),sep="\\\\") %>%
#   separate("Nation",c(NA,"Nation"),sep=" ") %>%
#   rename("Assists"="Ast") %>%
#   rename("TotalComp"="Cmp") %>%
#   rename("TotalAtt"="Att") %>%
#   rename("ShortComp"="Cmp_1") %>%
#   rename("ShortAtt"="Att_1") %>%
#   rename("MediumComp"="Cmp_2") %>%
#   rename("MediumAtt"="Att_2") %>%
#   rename("LongComp"="Cmp_3") %>%
#   rename("LongAtt"="Att_3") %>%
#   select(-"Rk",-"Cmp%",-"Cmp%_1",-"Cmp%_2",-"Cmp%_3")
# 
# EPLPlayerShooting <- EPLPlayerShooting_raw %>%
#   separate("Player",c("Player",NA),sep="\\\\") %>%
#   separate("Nation",c(NA,"Nation"),sep=" ") %>%
#   rename("Goals"="Gls") %>%
#   rename("PKGoals"="PK") %>%
#   rename("PKs"="PKatt")  %>%
#   select(-"Rk",-"SoT%")
# 
# EPLPlayerStats <- EPLPlayerStats_raw %>%
#   separate("Player",c("Player",NA),sep="\\\\") %>%
#   separate("Nation",c(NA,"Nation"),sep=" ") %>%
#   # rename("Played"="MP") %>%
#   # rename("Minutes"="Min") %>%
#   # rename("Goals"="Gls") %>%
#   # rename("Assists"="Ast") %>%
#   # rename("PKGoals"="PK") %>%
#   # rename("PKs"="PKatt") %>%
#   # rename("Fouls"="Fls") %>%
#   # rename("YC"="CrdY") %>%
#   # rename("RC"="CrdR") %>%
#   select(-"Rk",-("Gls_1":"G+A-PK"),-("xG_1":"npxG+xA"))
# 
# EPLTable <- EPLTable_raw %>%
#   # rename("Pos"="Rk") %>%
#   # rename("P"="Apps") %>%
#   # rename("GD"="GDiff") %>%
#   # rename("xGD"="xGDiff") %>%
#   select(-"Top Team Scorer",-"Goalkeeper")
# 
# EPLTeamPassing <- EPLTeamPassing_raw %>%
#   rename("Players"="# Pl") %>%
#   rename("Assists"="Ast") %>%
#   rename("ShotAssists"="KP") %>%
#   rename("TotalComp"="Cmp") %>%
#   rename("TotalAtt"="Att") %>%
#   rename("ShortComp"="Cmp_1") %>%
#   rename("ShortAtt"="Att_1") %>%
#   rename("MediumComp"="Cmp_2") %>%
#   rename("MediumAtt"="Att_2") %>%
#   rename("LongComp"="Cmp_3") %>%
#   rename("LongAtt"="Att_3") %>%
#   rename("FinalThird"="1/3") %>%
#   select(-"xA-A",-"Cmp%",-"Cmp%_1",-"Cmp%_2",-"Cmp%_3")
# 
# EPLTeamShooting <- EPLTeamShooting_raw %>%
#   rename("Players"="# Pl") %>%
#   rename("Goals"="Gls") %>%
#   rename("PKGoals"="PK") %>%
#   rename("PKs"="PKatt") %>%
#   rename("Shots"="Sh") %>%
#   select(-"SoT%")
# 
# EPLTeamStats <- EPLTeamStats_raw %>%
#   rename("Players"="# Pl") %>%
#   rename("Played"="MP") %>%
#   rename("Goals"="Gls") %>%
#   rename("Assists"="Ast") %>%
#   rename("PKGoals"="PK") %>%
#   rename("PKs"="PKatt") %>%
#   rename("YC"="CrdY") %>%
#   rename("RC"="CrdR") %>%
#   select(-("Starts":"Min"),-("Gls_1":"G+A-PK"),-("xG_1":"npxG+xA"))
# 
# SFCMatches <- SFCMatches_raw %>%
#   select(-c("Match Report","Notes"))
