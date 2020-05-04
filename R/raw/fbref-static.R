raw <- list()

raw[["fbref"]][["table"]][["2016"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/2016-2017-Premier-League-Stats",
                                                    extract=1,fix_columns=FALSE)
raw[["fbref"]][["table"]][["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/2017-2018-Premier-League-Stats",
                                                    extract=1,fix_columns=FALSE)
raw[["fbref"]][["table"]][["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/2018-2019-Premier-League-Stats",
                                                    extract=1,fix_columns=FALSE)

raw[["fbref"]][["matches"]][["2016"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/schedule/2016-2017-Premier-League-Fixtures",
                                                      extract=1,fix_columns=FALSE)
raw[["fbref"]][["matches"]][["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/schedule/2017-2018-Premier-League-Fixtures",
                                                      extract=1,fix_columns=FALSE)
raw[["fbref"]][["matches"]][["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/schedule/2018-2019-Premier-League-Fixtures",
                                                      extract=1,fix_columns=FALSE)

saveRDS(raw,file=here("data","fbref-static.rds"))
rm(raw)
# https://www.mitchhenderson.org/2020/04/how-sports-scientists-can-use-ggplot2-in-r-to-make-better-visualisations/
