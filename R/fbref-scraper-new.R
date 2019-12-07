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

squad_shooting_raw <- fbref_scrape(ncol=18,skip_head=0,fix_columns=TRUE,
                                   url="https://fbref.com/en/comps/9/shooting/Premier-League-Stats",
                                   nodes="#stats_shooting_squads .left , #stats_shooting_squads .right, #stats_shooting_squads .center")

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
