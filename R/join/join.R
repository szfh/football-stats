fbref <- readRDS(file=here("data","fbref-tidy.rds"))

squad <- fbref %>%
  filter(page=="squad") %>%
  select(-page) %>%
  unnest(cols=data) %>%
  group_by(stattype) %>%
  nest() %>%
  mutate(data=map(data,remove_empty,which="cols")) %$%
  data %>%
  reduce(full_join) %>%
  print

players <- fbref %>%
  filter(page=="player") %>%
  select(-page) %>%
  unnest(cols=data) %>%
  group_by(stattype) %>%
  nest() %>%
  mutate(data=map(data,remove_empty,which="cols")) %$%
  data %>%
  reduce(full_join)

matches <- fbref %>%
  filter(page=="schedule") %>%
  select(-page,-stattype) %>%
  unnest(cols=data)

# # read data
# tidy <- c(
#   readRDS(file=here("data","tidy-fbref.rds")),
#   readRDS(file=here("data","tidy-fivethirtyeight.rds"))
# )
# 
# # players
# players <- tidy[["fbref"]][["player"]] %>%
#   map(bind_rows, .id="Season") %>%
#   reduce(full_join)
# 
# # squad
# squad <- tidy[["fbref"]][["table"]] %>%
#   bind_rows(.id="Season") %>%
#   left_join(
#     tidy[["fbref"]][["squad"]] %>%
#       map(bind_rows, .id="Season") %>%
#       reduce(full_join)
#   )
# 
# # table
# # table <- tidy[["fbref"]][["table"]] %>%
# #   bind_rows(.id="Season")
# 
# # matches
# matches <- tidy[["fbref"]][["matches"]] %>%
#   bind_rows(.id="Season") %>%
#   left_join(tidy[["fivethirtyeight"]][["matches"]]) %>%
#   rename(
#     xGHomefbref=xGHome,
#     xGAwayfbref=xGAway,
#     xGHome538=xg1,
#     xGAway538=xg2,
#     nsxGHome538=nsxg1,
#     nsxGAway538=nsxg2
#   ) %>%
#   select(
#     "Wk":"Time",
#     "Home","Away",
#     "GoalsHome","GoalsAway",
#     "xGHomefbref","xGAwayfbref",
#     "xGHome538","xGAway538",
#     "spi1":"adj_score2",
#     everything()
#   )
# 
# matches_long <- matches %>%
#   pivot_longer(cols=c(Home,Away),
#                names_to="HA",
#                values_to="Team") %>%
#   left_join(matches) %>%
#   mutate(
#     Opposition=ifelse(HA=="Home",Away,Home),
#     GoalsF=ifelse(HA=="Home",GoalsHome,GoalsAway),
#     GoalsA=ifelse(HA=="Home",GoalsAway,GoalsHome),
#     xGFfbref=ifelse(HA=="Home",xGHomefbref,xGAwayfbref),
#     xGAfbref=ifelse(HA=="Home",xGAwayfbref,xGHomefbref),
#     xGF538=ifelse(HA=="Home",xGHome538,xGAway538),
#     xGA538=ifelse(HA=="Home",xGAway538,xGHome538),
#     spiF=ifelse(HA=="Home",spi1,spi2),
#     spiA=ifelse(HA=="Home",spi2,spi1),
#     probF=ifelse(HA=="Home",prob1,prob2),
#     probA=ifelse(HA=="Home",prob2,prob1),
#     proj_scoreF=ifelse(HA=="Home",proj_score1,proj_score2),
#     proj_scoreA=ifelse(HA=="Home",proj_score2,proj_score1),
#     importanceF=ifelse(HA=="Home",importance1,importance2),
#     importanceA=ifelse(HA=="Home",importance2,importance1),
#     nsxGF538=ifelse(HA=="Home",nsxGHome538,nsxGAway538),
#     nsxGA538=ifelse(HA=="Home",nsxGAway538,nsxGHome538),
#     adj_scoreF=ifelse(HA=="Home",adj_score1,adj_score2),
#     adj_scoreA=ifelse(HA=="Home",adj_score2,adj_score1)
#   ) %>%
#   select(
#     "Wk":"Time",
#     "HA":"adj_scoreA",
#     everything()
#   )
# 
# rm(tidy)
