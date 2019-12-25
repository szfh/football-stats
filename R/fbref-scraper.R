source("./R/fbref-scraper-functions.R")

raw <- list()

raw[["table"]] <- fbref_scrape(fbref_url="https://fbref.com/en/comps/9/Premier-League-Stats",extract=1)
# table_raw <- fbref_scrape(fbref_url="https://fbref.com/en/comps/9/Premier-League-Stats",extract=1)
# xpath='//*[@id="results32321_overall"]'
# css="table"

# xtable

# matches
raw[["matches"]] <- fbref_scrape(fbref_url="https://fbref.com/en/comps/9/schedule/Premier-League-Fixtures",extract=1)
# raw[["squad"]] <- list()
# squad stats
raw[["squad"]][["standard"]] <- fbref_scrape(fbref_url="https://fbref.com/en/comps/9/stats/Premier-League-Stats",extract=1,fix_columns=TRUE)
raw[["squad"]][["keepers"]] <- fbref_scrape(fbref_url="https://fbref.com/en/comps/9/keepers/Premier-League-Stats",extract=1,fix_columns=TRUE)
raw[["squad"]][["shooting"]] <- fbref_scrape(fbref_url="https://fbref.com/en/comps/9/shooting/Premier-League-Stats",extract=1,fix_columns=FALSE)
raw[["squad"]][["passing"]] <- fbref_scrape(fbref_url="https://fbref.com/en/comps/9/passing/Premier-League-Stats",extract=1,fix_columns=TRUE)
raw[["squad"]][["playingtime"]] <- fbref_scrape(fbref_url="https://fbref.com/en/comps/9/playingtime/Premier-League-Stats",extract=1,fix_columns=TRUE)
raw[["squad"]][["misc"]] <- fbref_scrape(fbref_url="https://fbref.com/en/comps/9/misc/Premier-League-Stats",extract=1,fix_columns=FALSE)

# as list - http://www.r-tutor.com/r-introduction/list/named-list-members
# how to make a list of data frames https://stackoverflow.com/questions/17499013/how-do-i-make-a-list-of-data-frames

# player stats
# player_standard_raw <- fbref_scrape(ncol=10,skip_head=0,fix_columns=TRUE,
#                                     fbref_url="https://fbref.com/en/comps/9/stats/Premier-League-Stats",
#                                    nodes="#all_stats_standard .center , #all_stats_standard .left, #all_stats_standard .right")

# https://github.com/moisesvasquezca/EPLPredictor/tree/b082999e700e0854c0686a0d8a48cb7ad094d8e4

# player_keepers_raw <- fbref_scrape(ncol=10,skip_head=0,fix_columns=F,
#                                   url="https://fbref.com/en/comps/9/keepers/Premier-League-Stats",
#                                   nodes="#stats_keeper .left , #stats_keeper .right, #stats_keeper .center")
# 
# player_shooting_raw <- fbref_scrape(ncol=10,skip_head=0,fix_columns=F,
#                                    url="https://fbref.com/en/comps/9/shooting/Premier-League-Stats",
#                                    nodes="#stats_shooting .left , #stats_shooting .right, #stats_shooting .center")
# 
# player_passing_raw <- fbref_scrape(ncol=10,skip_head=0,fix_columns=F,
#                                   url="https://fbref.com/en/comps/9/passing/Premier-League-Stats",
#                                   nodes="#stats_passing .left , #stats_passing .right, #stats_passing .center")
# 
# player_playingtime_raw <- fbref_scrape(ncol=10,skip_head=0,fix_columns=F,
#                                       url="https://fbref.com/en/comps/9/playingtime/Premier-League-Stats",
#                                       nodes="#stats_playing_time .left , #stats_playing_time .right, #stats_playing_time .center")
# 
# player_misc_raw <- fbref_scrape(ncol=10,skip_head=0,fix_columns=F,
#                                url="https://fbref.com/en/comps/9/misc/Premier-League-Stats",
#                                nodes="#stats_misc .left , #stats_misc .right, #stats_misc .center")

# fbref_scrape(ncol=1,skip_head=0,fix_columns=F,
#                                    url="https://fbref.com/en/comps/9/stats/Premier-League-Stats",
#                                    nodes="")

# data <-
#   url %>%
#   read_html() %>%
#   html_nodes(as.character(nodes)) %>%
#   html_text %>%
#   matrix(ncol=ncol,byrow=T) %>%
#   as_tibble(.name_repair="minimal")

# "https://fbref.com/en/comps/9/stats/Premier-League-Stats" %>%
#   read_html() %>%
#   html_nodes("#stats_standard_squads .left , #stats_standard_squads .right, #stats_standard_squads .center") %>%
#   html_text %>%
#   # slice(-5) %>%
#   View()
