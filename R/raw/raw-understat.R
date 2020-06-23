source(here("R","raw","raw-utils.R"))

.eplseasons <- tribble(~season,
                       "2019",
                       # "2018",
)

.datatypes_1 <- tribble(~datatype,
                       "league",
)

.tables_1 <- tribble(~stattype, ~statselector,
                     "schedule","datesData",
                     "players","playersData",
                     # "teams","teamsData",
)

understat <- data.frame() %>%
  bind_rows(crossing(.datatypes_1,.tables_1)) %>%
  crossing(.eplseasons) %>%
  print

understat %<>%
  mutate(data=pmap(list("EPL",season,statselector),possibly(understat_scrape_league, otherwise=NA))) %>%
  print
