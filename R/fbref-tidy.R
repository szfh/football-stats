EPLMatches <- EPLMatches_raw %>%
  separate("Score",c("GH","GA"),sep="[:punct:]") %>%
  rename("xGH"="xG") %>%
  rename("xGA"="xG_1") %>%
  filter(!is.na(Wk)) %>%
  select(-c("Match Report","Notes"))

EPLPlayerShooting <- EPLPlayerShooting_raw %>%
  separate("Player",c("Player",NA),sep="\\\\") %>%
  separate("Nation",c(NA,"Nation"),sep=" ") %>%
  rename("Goals"="Gls") %>%
  rename("PKGoals"="PK") %>%
  rename("PKs"="PKatt")  %>%
  select(-("SoT%":"G/SoT"),-("npxG/Sh":"np:G-xG"))

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
  select(-"Rk",-"Mn/Ap",-("Gls_1":"npxG+xA"))

EPLTable <- EPLTable_raw %>%
  rename("Pos"="Rk") %>%
  rename("P"="Apps") %>%
  rename("GD"="GDiff") %>%
  rename("xGD"="xGDiff") %>%
  select(-"Top Team Scorer",-"Goalkeeper")

EPLTeamShooting <- EPLTeamShooting_raw %>%
  rename("Players"="# Pl") %>%
  rename("Goals"="Gls") %>%
  rename("PKGoals"="PK") %>%
  rename("PKs"="PKatt") %>%
  rename("Shots"="Sh") %>%
  select(-"SoT%")

EPLTeamStats <- EPLTeamStats_raw %>%
  rename("Players"="# Pl") %>%
  rename("Played"="MP") %>%
  rename("Goals"="Gls") %>%
  rename("Assists"="Ast") %>%
  rename("PKGoals"="PK") %>%
  rename("PKs"="PKatt") %>%
  rename("YC"="CrdY") %>%
  rename("RC"="CrdR") %>%
  select(-("Starts":"Min"),-("Gls_1":"G+A-PK"),-("xG_1":"npxG+xA"))

SFCMatches <- SFCMatches_raw %>%
  select(-c("Match Report","Notes"))