.eplseasons <- tribble(~season, ~code,
                       "2019-20",NA
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
  mutate(page_url=glue("https://fbref.com/en/comps/9/{page}/")) %>%
  mutate(content_selector_id=glue("%23stats_{table}{selector}")) %>%
  mutate(data = map2(page_url, content_selector_id, possibly(fbref_scrape, otherwise=NA)))

saveRDS(fbref,file=here("data","fbref-raw-dynamic.rds"))
rm(fbref)
