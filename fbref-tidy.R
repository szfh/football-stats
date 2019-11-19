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