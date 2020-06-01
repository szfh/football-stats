.eplseasons <- tribble(~season, ~code, # advanced/nonadvanced?
                       "2018-19",1889,
                       "2017-18",1631,
                       # "2016-17",1526,
)

.pages <- tribble(~page, ~table,
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

.datatype <- tribble(~type, ~selector,
                     "squad","_squads",
                     "player","")

fbref <- .eplseasons %>%
  crossing(.pages) %>%
  crossing(.datatype)

fbref %<>%
  mutate(page_url=glue("https://fbref.com/en/comps/9/{code}/{page}/")) %>%
  mutate(content_selector_id=glue("%23stats_{table}{selector}")) %>%
  mutate(data = map2(page_url, content_selector_id, possibly(fbref_scrape, otherwise=NA)))

saveRDS(fbref,file=here("data","fbref-test.rds"))
rm(fbref)
