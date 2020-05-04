raw <- readRDS(file=here("data","fbref-static.rds"))

raw[["fbref"]][["table"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/Premier-League-Stats",
                                          extract=1,fix_columns=FALSE)

raw[["fbref"]][["matches"]][["2019"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/schedule/Premier-League-Fixtures",
                                                      extract=1,fix_columns=FALSE)

raw[["fbref"]][["squad"]][["stats"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/stats/Premier-League-Stats",
                                                     extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["keepers"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/keepers/Premier-League-Stats",
                                                       extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["keepersadv"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/keepersadv/Premier-League-Stats",
                                                          extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["shooting"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/shooting/Premier-League-Stats",
                                                        extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["passing"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/passing/Premier-League-Stats",
                                                       extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["passingtypes"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/passing_types/Premier-League-Stats",
                                                            extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["gca"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/gca/Premier-League-Stats",
                                                   extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["defense"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/defense/Premier-League-Stats",
                                                       extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["possession"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/possession/Premier-League-Stats",
                                                          extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["playingtime"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/playingtime/Premier-League-Stats",
                                                           extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["misc"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/misc/Premier-League-Stats",
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
