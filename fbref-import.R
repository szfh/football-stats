EPLTable_raw <- readr::read_csv("./data/fbref/2019/EPLTable.txt",comment="##") # https://fbref.com/en/comps/9/Premier-League-Stats#results32321::none
EPLTeamStats_raw <- readr::read_csv("./data/fbref/2019/EPLTeamStats.txt",comment="##",skip=1) # https://fbref.com/en/comps/9/stats/Premier-League-Stats#stats_player_teams::none
EPLPlayerStats_raw <- readr::read_csv("./data/fbref/2019/EPLPlayerStats.txt",comment="##",skip=1) # https://fbref.com/en/comps/9/stats/Premier-League-Stats#stats_player::none
SFCMatches_raw <- readr::read_csv("./data/fbref/2019/SFCMatches.txt",comment="##") # https://fbref.com/en/squads/33c895d4/Southampton#ks_sched_all::none
# SFCPlayerStats_raw <- readr::read_csv("./data/fbref/2019/SFCPlayerStats.txt",comment="##",skip=1) # https://fbref.com/en/squads/33c895d4/Southampton#stats_player::none

EPLTable <- EPLTable_raw %>%
  rename("Pos"="Rk") %>%
  rename("P"="Apps") %>%
  rename("GD"="GDiff") %>%
  rename("xGD"="xGDiff") %>%
  select(-c("Top Team Scorer",Goalkeeper)) %>%
  View("EPLTable")

EPLTeamStats <- EPLTeamStats_raw %>%
  rename("Players"="# Pl") %>%
  rename("Played"="Apps") %>%
  rename("Goals"="Gls") %>%
  rename("Assists"="Ast") %>%
  rename("PKGoals"="PK") %>%
  rename("PKs"="PKatt") %>%
  rename("Fouls"="Fls") %>%
  rename("YC"="CrdY") %>%
  rename("RC"="CrdR") %>%
  select(-("Gls_1":"Crd")) %>%
  View("EPLTeamStats")

EPLPlayerStats <- EPLPlayerStats_raw %>%
  separate("Player",c("Player",NA),sep="\\\\") %>%
  separate("Nation",c(NA,"Nation"),sep=" ") %>%
  rename("Played"="Apps") %>%
  rename("Minutes"="Min") %>%
  rename("Goals"="Gls") %>%
  rename("Assists"="Ast") %>%
  rename("PKGoals"="PK") %>%
  rename("PKs"="PKatt") %>%
  rename("Fouls"="Fls") %>%
  rename("YC"="CrdY") %>%
  rename("RC"="CrdR") %>%
  select(-c("Rk","Mn/Ap","Gls_1":"npxG+xA")) %>%
  View("EPLPlayerStats")