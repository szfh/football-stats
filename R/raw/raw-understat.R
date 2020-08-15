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
  filter(season!=2020)

understat_new <-
  anti_join(understat_all, understat_keep) %>%
  mutate(data=pmap(list("EPL",season,statselector),possibly(understat_scrape_league, otherwise=NA))) %>%
  print

###
.match_id <- understat_saved %>%
  filter(stattype=="schedule") %>%
  unnest(cols=data) %>%
  select(season,id,isResult) %>%
  # select(-c(datatype,stattype,statselector)) %>%
  glimpse

.match_data <- tribble(~datatype,
                       "stats",
                       "shots"
)

# match_id_scrape <-
#   crossing(.match_id,.match_data) %>%
#   filter(datatype=="stats") %>%
#   filter(isResult==TRUE)

understat_all2 <- data.frame() %>%
  bind_rows(crossing(.match_id,.match_data) %>%
              filter(isResult==TRUE)) %>%
  glimpse

understat_keep2 <- understat_all2 %>%
  filter(season!=2019)

understat_new2 <-
  anti_join(understat_all2, understat_keep2) %>%
  slice(1:20)

understat_new2 <- understat_new2 %>%
  mutate(data=pmap(list(datatype,id),possibly(understat_scrape_match, otherwise=NA)))
# saveRDS(match_id_scrape,file=here("data","understat-test1.rds"))

###

understat <- bind_rows(understat_keep, understat_new, understat_new2) %>%
  filter(!is.na(data)) %>%
  relocate(data,.after=last_col())

# understat <- bind_rows(understat_keep, understat_new) %>%
#   filter(!is.na(data))

saveRDS(understat,here("data","understat-raw.rds"))
