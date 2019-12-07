# write.csv(matches_raw,file="data/fbref/matches_raw.csv",row.names=F)


table_raw <- fbref_scrape(url="https://fbref.com/en/comps/9/Premier-League-Stats",ncol=19,fix_columns=TRUE,
                            nodes="#results32321_overall .center , #results32321_overall .right, #results32321_overall .left, #results32321_overall .right")