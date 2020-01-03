tidy[["player"]][["standard"]] <- raw[["player"]][["standard"]] %>%
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

tidy[["player"]][["passing"]] <- raw[["player"]][["passing"]] %>%
  rename(
    "TotalCmp"="Cmp",
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
  ) %>%
  select(
    -"Rk",
    -"Ast",
    -"xA",
  )

tidy[["player"]][["shooting"]] <- raw[["player"]][["shooting"]] %>%
  select(
    -"Rk",
    -"Gls",
    -"PK",
    -"PKatt",
    -"xG",
    -"npxG",
    -"90s",
    -"FK",
  )

tidy[["player"]][["misc"]] <- raw[["player"]][["misc"]] %>%
  select(
    -"Rk",
    -"CrdY",
    -"CrdR",
    -"90s",
  )

tidy[["player"]][["playingtime"]] <- raw[["player"]][["playingtime"]] %>%
  rename(
    "xGOn-Off"="On-Off_1"
  ) %>%
  select(
    -"Rk",
    -"MP",
    -"Starts",
    -"Min",
    -"90s",
    -"Matches",
  )

players <- reduce(tidy[["player"]],left_join) %>%
  separate("Player",c("Player",NA),sep="\\\\") %>%
  separate("Nation",c(NA,"Nation"),sep=" ") %>%
  separate("Pos",c("Pos1","Pos2"),sep=2)