source(here("R","raw","raw-utils.R"))

.eplseasons <- tribble(~season,
                       "2019",
                       "2018",
)

.datatypes_1 <- tribble(~datatype,
                       "league",
)

.tables_1 <- tribble(~stattype, ~statselector,
                     "schedule","datesData",
                     "players","playersData",
                     # "teams","teamsData",
)

understat_saved <- readRDS(here("data","understat-raw.rds"))

understat_all <- data.frame() %>%
  bind_rows(crossing(.datatypes_1,.tables_1)) %>%
  crossing(.eplseasons) %>%
  print

understat_keep <- understat_saved %>%
  filter(season!=2019)

understat_new <-
  anti_join(understat_all, understat_keep) %>%
  mutate(data=pmap(list("EPL",season,statselector),possibly(understat_scrape_league, otherwise=NA))) %>%
  print

understat <- bind_rows(understat_keep, understat_new) %>%
  filter(!is.na(data))

saveRDS(understat,here("data","understat-raw.rds"))
