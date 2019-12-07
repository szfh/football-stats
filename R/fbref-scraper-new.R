# table
table_raw <- fbref_scrape(url="https://fbref.com/en/comps/9/Premier-League-Stats",ncol=19,fix_columns=TRUE,
                            nodes="#results32321_overall .center , #results32321_overall .right, #results32321_overall .left, #results32321_overall .right")

# matches
matches_raw <- fbref_scrape(url="https://fbref.com/en/comps/9/schedule/Premier-League-Fixtures",ncol=14,fix_columns=TRUE,
                            nodes=".left , .right, .center")

# squad stats
standard_squad <- fbref_scrape(url="https://fbref.com/en/comps/9/stats/Premier-League-Stats",ncol=1,
             nodes="#stats_standard_squads .left , #stats_standard_squads .right, #stats_standard_squads .center")


# fbref_scrape(url="",ncol=,fix_columns=FALSE,
#              nodes="")

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
