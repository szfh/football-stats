fbref_static <- readRDS(file=here("data","fbref-raw-static.rds"))

.EPLseasons <- tribble(~Season, ~Code, # advanced/nonadvanced?
                       "2019-20",NA
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
  mutate(page_url=glue("https://fbref.com/en/comps/9/{Page}/")) %>%
  mutate(content_selector_id=glue("%23stats_{Table}{Selector}")) %>%
  mutate(data = map2(page_url, content_selector_id, possibly(fbref_scrape, otherwise=NA))) %>%
  print

fbref <- bind_rows(fbref_static,fbref)

saveRDS(fbref,file=here("data","fbref-raw.rds"))
rm(fbref_static,fbref)
