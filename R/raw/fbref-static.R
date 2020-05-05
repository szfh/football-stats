raw <- list()

# raw[["fbref"]][["table"]][["2016"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1526/",
#                                                     extract=1,fix_columns=FALSE)
raw[["fbref"]][["table"]][["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1631/",
                                                    extract=1,fix_columns=FALSE)
raw[["fbref"]][["table"]][["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/",
                                                    extract=1,fix_columns=FALSE)

# raw[["fbref"]][["matches"]][["2016"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1526/schedule/",
#                                                       extract=1,fix_columns=FALSE)
raw[["fbref"]][["matches"]][["2017"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1631/schedule/",
                                                      extract=1,fix_columns=FALSE)
raw[["fbref"]][["matches"]][["2018"]] <- fbref_scrape(url="https://fbref.com/en/comps/9/1889/schedule/",
                                                      extract=1,fix_columns=FALSE)

saveRDS(raw,file=here("data","fbref-static.rds"))
rm(raw)
# https://www.mitchhenderson.org/2020/04/how-sports-scientists-can-use-ggplot2-in-r-to-make-better-visualisations/
