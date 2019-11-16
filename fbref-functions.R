url_fbref <- "https://fbref.com/en/"
url_epl_1920 <- "https://fbref.com/en/comps/9/Premier-League-Stats"

# scraped_page <- read_html(url_epl_1920)

# scraped_page %>%
#   html_nodes("td") %>%
#   html_text %>%
#   View()

EPLTable <- readr::read_csv("./data/fbref/2019/EPLTable.txt") # https://fbref.com/en/comps/9/Premier-League-Stats#results32321::none
EPLTeamStats <- readr::read_csv("./data/fbref/2019/EPLTeamStats.txt") # https://fbref.com/en/comps/9/stats/Premier-League-Stats#stats_player_teams::none
EPLPlayerStats <- readr::read_csv("./data/fbref/2019/EPLPlayerStats.txt") # https://fbref.com/en/comps/9/stats/Premier-League-Stats#stats_player::none
SFCMatches <- readr::read_csv("./data/fbref/2019/SFCMatches.txt") # https://fbref.com/en/squads/33c895d4/Southampton#ks_sched_all::none
SFCPlayerStats <- readr::read_csv("./data/fbref/2019/SFCPlayerStats.txt") # https://fbref.com/en/squads/33c895d4/Southampton#stats_player::none