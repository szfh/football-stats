raw <- list()

raw[["fbref"]][["table"]][["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1631/",
                                                    extract=1,fix_columns=FALSE)
raw[["fbref"]][["table"]][["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/",
                                                    extract=1,fix_columns=FALSE)

raw[["fbref"]][["matches"]][["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1631/schedule/",
                                                      extract=1,fix_columns=FALSE)
raw[["fbref"]][["matches"]][["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/schedule/",
                                                      extract=1,fix_columns=FALSE)

raw[["fbref"]][["squad"]][["stats"]][["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1631/stats/",
                                                               extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["stats"]][["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/stats/",
                                                               extract=1,fix_columns=TRUE)

raw[["fbref"]][["squad"]][["keepers"]][["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1631/keepers/",
                                                                 extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["keepers"]][["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/keepers/",
                                                                 extract=1,fix_columns=TRUE)

raw[["fbref"]][["squad"]][["keepersadv"]][["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1631/keepersadv/",
                                                                    extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["keepersadv"]][["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/keepersadv/",
                                                                    extract=1,fix_columns=TRUE)

raw[["fbref"]][["squad"]][["shooting"]][["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1631/shooting/",
                                                                  extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["shooting"]][["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/shooting/",
                                                                  extract=1,fix_columns=TRUE)

raw[["fbref"]][["squad"]][["passing"]][["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1631/passing/",
                                                                 extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["passing"]][["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/passing/",
                                                                 extract=1,fix_columns=TRUE)

raw[["fbref"]][["squad"]][["passingtypes"]][["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1631/passing_types/",
                                                                      extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["passingtypes"]][["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/passing_types/",
                                                                      extract=1,fix_columns=TRUE)

raw[["fbref"]][["squad"]][["gca"]][["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1631/gca/",
                                                             extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["gca"]][["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/gca/",
                                                             extract=1,fix_columns=TRUE)

raw[["fbref"]][["squad"]][["defense"]][["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1631/defense/",
                                                                 extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["defense"]][["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/defense/",
                                                                 extract=1,fix_columns=TRUE)

raw[["fbref"]][["squad"]][["possession"]][["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1631/possession/",
                                                                    extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["possession"]][["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/possession/",
                                                                    extract=1,fix_columns=TRUE)

raw[["fbref"]][["squad"]][["playingtime"]][["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1631/playingtime/",
                                                                     extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["playingtime"]][["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/playingtime/",
                                                                     extract=1,fix_columns=TRUE)

raw[["fbref"]][["squad"]][["misc"]][["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1631/misc/",
                                                              extract=1,fix_columns=TRUE)
raw[["fbref"]][["squad"]][["misc"]][["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/misc/",
                                                              extract=1,fix_columns=TRUE)

saveRDS(raw,file=here("data","fbref-static.rds"))
rm(raw)
# https://www.mitchhenderson.org/2020/04/how-sports-scientists-can-use-ggplot2-in-r-to-make-better-visualisations/
