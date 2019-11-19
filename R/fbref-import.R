# import from ./data/fbref/2019/
EPLMatches_raw <- readr::read_csv("./data/fbref/2019/EPLMatches.txt",comment="##") # https://fbref.com/en/comps/9/schedule/Premier-League-Fixtures
# EPLPassing_raw <- readr::read_csv("./data/fbref/2019/EPLPassing.txt",comment="##") # 
EPLPlayerShooting_raw <- readr::read_csv("./data/fbref/2019/EPLPlayerShooting.txt",comment="##") #
EPLPlayerStats_raw <- readr::read_csv("./data/fbref/2019/EPLPlayerStats.txt",comment="##",skip=1) # https://fbref.com/en/comps/9/stats/Premier-League-Stats
EPLTable_raw <- readr::read_csv("./data/fbref/2019/EPLTable.txt",comment="##") # https://fbref.com/en/comps/9/Premier-League-Stats
EPLTeamShooting_raw <- readr::read_csv("./data/fbref/2019/EPLTeamShooting.txt",comment="##") #
EPLTeamStats_raw <- readr::read_csv("./data/fbref/2019/EPLTeamStats.txt",comment="##",skip=1) # https://fbref.com/en/comps/9/stats/Premier-League-Stats
SFCMatches_raw <- readr::read_csv("./data/fbref/2019/SFCMatches.txt",comment="##") # https://fbref.com/en/squads/33c895d4/Southampton

# tidy
EPLTable <- EPLTable_raw %>%
  rename("Pos"="Rk") %>%
  rename("P"="Apps") %>%
  rename("GD"="GDiff") %>%
  rename("xGD"="xGDiff") %>%
  select(-c("Top Team Scorer",Goalkeeper))

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
  select(-("Gls_1":"Crd"))

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
  select(-c("Rk","Mn/Ap","Gls_1":"npxG+xA"))

EPLMatches <- EPLMatches_raw %>%
  separate("Score",c("GH","GA"),sep="[:punct:]") %>%
  rename("xGH"="xG") %>%
  rename("xGA"="xG_1") %>%
  filter(!is.na(Wk)) %>%
  select(-c("Match Report","Notes"))

SFCMatches <- SFCMatches_raw %>%
  select(-c("Match Report","Notes"))

EPLTeamShooting <- EPLTeamShooting_raw %>%
  rename("Players"="# Pl") %>%
  rename("Goals"="Gls") %>%
  rename("PKGoals"="PK") %>%
  rename("PKs"="PKatt") %>%
  rename("Shots"="Sh") %>%
  select(-"SoT%")

EPLPlayerShooting <- EPLPlayerShooting_raw %>%
  separate("Player",c("Player",NA),sep="\\\\") %>%
  separate("Nation",c(NA,"Nation"),sep=" ") %>%
  rename("Goals"="Gls") %>%
  rename("PKGoals"="PK") %>%
  rename("PKs"="PKatt") %>%
  View("PlayerShooting")
