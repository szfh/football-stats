raw <- list()
raw[["table"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/Premier-League-Stats",
                               extract=1,fix_columns=FALSE)
raw[["matches"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/schedule/Premier-League-Fixtures",
                                 extract=1,fix_columns=FALSE)
raw[["squad"]][["standard"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/stats/Premier-League-Stats",
                                             extract=1,fix_columns=TRUE)
raw[["squad"]][["keepers"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/keepers/Premier-League-Stats",
                                            extract=1,fix_columns=TRUE)
raw[["squad"]][["shooting"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/shooting/Premier-League-Stats",
                                             extract=1,fix_columns=FALSE)
raw[["squad"]][["passing"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/passing/Premier-League-Stats",
                                            extract=1,fix_columns=TRUE)
raw[["squad"]][["playingtime"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/playingtime/Premier-League-Stats",
                                                extract=1,fix_columns=TRUE)
raw[["squad"]][["misc"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/misc/Premier-League-Stats",
                                         extract=1,fix_columns=TRUE)

raw[["player"]][["standard"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/stats/Premier-League-Stats",
                                              comment=TRUE,extract=1,fix_columns=TRUE)
raw[["player"]][["keepers"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/keepers/Premier-League-Stats",
                                             comment=TRUE,extract=1,fix_columns=TRUE)
raw[["player"]][["shooting"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/shooting/Premier-League-Stats",
                                              comment=TRUE,extract=1,fix_columns=FALSE)
raw[["player"]][["passing"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/passing/Premier-League-Stats",
                                             comment=TRUE,extract=1,fix_columns=TRUE)
raw[["player"]][["playingtime"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/playingtime/Premier-League-Stats",
                                                 comment=TRUE,extract=1,fix_columns=TRUE)
raw[["player"]][["misc"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/misc/Premier-League-Stats",
                                          comment=TRUE,extract=1,fix_columns=TRUE)