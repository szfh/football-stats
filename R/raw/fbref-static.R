old_matches <- list()

old_matches[["2016"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/schedule/2016-2017-Premier-League-Fixtures",
                                      extract=1,fix_columns=FALSE)
old_matches[["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/schedule/2017-2018-Premier-League-Fixtures",
                                      extract=1,fix_columns=FALSE)
old_matches[["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/schedule/2018-2019-Premier-League-Fixtures",
                                            extract=1,fix_columns=FALSE)

saveRDS(old_matches,file=here("data","oldmatches-fbref.rds"))
