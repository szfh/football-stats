.eplseasons <- tribble(~season, ~seasoncode, #advanced/non-advanced?
                       #"2019-20",3232
                       "2018-19",1889,
                       "2017-18",1631,
                       # "2016-17",1526,
)

.datatypes_1 <- tribble(~page,
                        "player",
                        "squad",
)

.tables_1 <- tribble(~stattype, ~statselector,
                     "stats","standard",
                     "keepers","keeper",
                     "keepersadv","keeper_adv",
                     "shooting","shooting",
                     "passing","passing",
                     "passing_types","passing_types",
                     "gca","gca",
                     "defense","defense",
                     "possession","possession",
                     "playingtime","playing_time",
                     "misc","misc",
)

.datatypes_2 <- tribble(~page,
                        "schedule",
)

fbref <- data.frame() %>%
  bind_rows(crossing(.datatypes_1,.tables_1)) %>% #players and squads * datatypes
  bind_rows(.datatypes_2) %>% #fixtures
  crossing(.eplseasons)

fbref %<>%
  mutate(page_url=fbref_get_url(page,seasoncode,stattype,statselector)) %>%
  mutate(content_selector_id=fbref_get_selector(page,seasoncode,stattype,statselector))

fbref %<>%
  # mutate(data=fbref_scrape(page_url, content_selector_id)) %>% # doesnt work
  mutate(data = map2(page_url, content_selector_id, possibly(fbref_scrape, otherwise=NA)))

saveRDS(fbref,file=here("data","fbref-raw-static.rds"))
rm(fbref)
