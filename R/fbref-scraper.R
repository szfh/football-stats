source("./R/fbref-scraper-functions.R")
# table
table_raw <- fbref_scrape(ncol=19,fix_columns=TRUE,
                          url="https://fbref.com/en/comps/9/Premier-League-Stats",
                          nodes="#results32321_overall .center , #results32321_overall .right, #results32321_overall .left, #results32321_overall .right")

# matches
matches_raw <- fbref_scrape(ncol=14,fix_columns=TRUE,remove_last_row=TRUE,
                            url="https://fbref.com/en/comps/9/schedule/Premier-League-Fixtures",
                            nodes=".left , .right, .center")

# squad stats
squad_standard_raw <- fbref_scrape(ncol=24,skip_head=6,fix_columns=TRUE,
                               url="https://fbref.com/en/comps/9/stats/Premier-League-Stats",
                               nodes="#stats_standard_squads .left , #stats_standard_squads .right, #stats_standard_squads .center")

squad_keepers_raw <- fbref_scrape(ncol=15,skip_head=0,fix_columns=TRUE,
                                      url="https://fbref.com/en/comps/9/keepers/Premier-League-Stats",
                                      nodes="#stats_keeper_squads .left , #stats_keeper_squads .right, #stats_keeper_squads .center")

squad_shooting_raw <- fbref_scrape(ncol=18,skip_head=0,fix_columns=TRUE,
                                   url="https://fbref.com/en/comps/9/shooting/Premier-League-Stats",
                                   nodes="#stats_shooting_squads .left , #stats_shooting_squads .right, #stats_shooting_squads .center")

squad_passing_raw <- fbref_scrape(ncol=26,skip_head=7,fix_columns=TRUE,
                                   url="https://fbref.com/en/comps/9/passing/Premier-League-Stats",
                                   nodes="#stats_passing_squads .left , #stats_passing_squads .right, #stats_passing_squads .center")

squad_playingtime_raw <- fbref_scrape(ncol=20,skip_head=6,fix_columns=TRUE,
                                  url="https://fbref.com/en/comps/9/playingtime/Premier-League-Stats",
                                  nodes="#stats_playing_time_squads .left , #stats_playing_time_squads .right, #stats_playing_time_squads .center")

squad_misc_raw <- fbref_scrape(ncol=14,skip_head=0,fix_columns=TRUE,
                                      url="https://fbref.com/en/comps/9/misc/Premier-League-Stats",
                                      nodes="#stats_misc_squads .left , #stats_misc_squads .right, #stats_misc_squads .center")

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
