raw <- readRDS(file=here("data","fbref-static.rds"))

raw[["fbref"]][["table"]][["2019"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/Premier-League-Stats",
                                          extract=1,fix_columns=FALSE)

raw[["fbref"]][["matches"]][["2019"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/schedule/Premier-League-Fixtures",
                                                      extract=1,fix_columns=FALSE)

raw[["fbref"]][["squad"]][["stats"]][["2019"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/stats/Premier-League-Stats",
                                                     extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["keepers"]][["2019"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/keepers/Premier-League-Stats",
                                                       extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["keepersadv"]][["2019"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/keepersadv/Premier-League-Stats",
                                                          extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["shooting"]][["2019"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/shooting/Premier-League-Stats",
                                                        extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["passing"]][["2019"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/passing/Premier-League-Stats",
                                                       extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["passingtypes"]][["2019"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/passing_types/Premier-League-Stats",
                                                            extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["gca"]][["2019"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/gca/Premier-League-Stats",
                                                   extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["defense"]][["2019"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/defense/Premier-League-Stats",
                                                       extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["possession"]][["2019"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/possession/Premier-League-Stats",
                                                          extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["playingtime"]][["2019"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/playingtime/Premier-League-Stats",
                                                           extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["misc"]][["2019"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/misc/Premier-League-Stats",
                                                    extract=1,fix_columns=TRUE)

raw[["fbref"]][["player"]][["stats"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/stats/Premier-League-Stats",
                                                      comment=TRUE,extract=1,fix_columns=TRUE)
raw[["fbref"]][["player"]][["keepers"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/keepers/Premier-League-Stats",
                                                        comment=TRUE,extract=1,fix_columns=TRUE)
raw[["fbref"]][["player"]][["keepersadv"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/keepersadv/Premier-League-Stats",
                                                           comment=TRUE,extract=1,fix_columns=TRUE)
raw[["fbref"]][["player"]][["shooting"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/shooting/Premier-League-Stats",
                                                         comment=TRUE,extract=1,fix_columns=TRUE)
raw[["fbref"]][["player"]][["passing"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/passing/Premier-League-Stats",
                                                        comment=TRUE,extract=1,fix_columns=TRUE)
raw[["fbref"]][["player"]][["passingtypes"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/passing_types/Premier-League-Stats",
                                                             comment=TRUE,extract=1,fix_columns=TRUE)
raw[["fbref"]][["player"]][["gca"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/gca/Premier-League-Stats",
                                                    comment=TRUE,extract=1,fix_columns=TRUE)
raw[["fbref"]][["player"]][["defense"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/defense/Premier-League-Stats",
                                                        comment=TRUE,extract=1,fix_columns=TRUE)
raw[["fbref"]][["player"]][["possession"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/possession/Premier-League-Stats",
                                                           comment=TRUE,extract=1,fix_columns=TRUE)
raw[["fbref"]][["player"]][["playingtime"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/playingtime/Premier-League-Stats",
                                                            comment=TRUE,extract=1,fix_columns=TRUE)
raw[["fbref"]][["player"]][["misc"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/misc/Premier-League-Stats",
                                                     comment=TRUE,extract=1,fix_columns=TRUE)

saveRDS(raw,file=here("data","raw-fbref.rds"))
rm(raw)
