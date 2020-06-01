.EPLseasons <- tribble(~Season, ~Code, # advanced/nonadvanced?
                       "2018-19",1889,
                       "2017-18",1631,
                       # "2016-17",1526,
)

.Pages <- tribble(~Page, ~Table,
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

.Datatype <- tribble(~Type, ~Selector,
                     "squad","_squads",
                     "player","")

fbref <- .EPLseasons %>%
  crossing(.Pages) %>%
  crossing(.Datatype)

fbref %<>%
  mutate(page_url=glue("https://fbref.com/en/comps/9/{Code}/{Page}/")) %>%
  mutate(content_selector_id=glue("%23stats_{Table}{Selector}")) %>%
  mutate(data = map2(page_url, content_selector_id, possibly(fbref_scrape, otherwise=NA)))

saveRDS(fbref,file=here("data","fbref-test.rds"))
rm(fbref)
