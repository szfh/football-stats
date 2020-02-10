source(here::here("R","fbref","library.R"))

raw <- readRDS(file=here("data","fbref","raw.rds"))
tidy <- list()
source(here("R","fbref","tidy-players.R"))
source(here("R","fbref","tidy-teams.R"))
source(here("R","fbref","tidy-matches.R"))

players <- reduce(tidy[["player"]],full_join) %>%
  separate("Player",c("Player",NA),sep="\\\\",fill="right") %>%
  separate("Nation",c(NA,"Nation"),sep=" ",fill="right") %>%
  separate("Pos",c("Pos1",NA,"Pos2"),sep=c(2,3),fill="right")

squad <- tidy[["table"]] %>%
  full_join(reduce(tidy[["squad"]],full_join))

matches <- tidy[["matches"]]

matches_long <- tidy[["matches"]] %>%
  pivot_longer(cols=c(Home,Away),
               names_to="HA",
               values_to="Team") %>%
  left_join(matches) %>%
  mutate(
    Opposition=ifelse(HA=="Home",Away,Home),
    GF=ifelse(HA=="Home",GoalsHome,GoalsAway),
    GA=ifelse(HA=="Home",GoalsAway,GoalsHome),
    xGF=ifelse(HA=="Home",xGHome,xGAway),
    xGA=ifelse(HA=="Home",xGAway,xGHome),
  ) %>%
  select(
    -("xGHome":"xGAway"),
    -("Home":"Away"),
  )

rm(raw,tidy)