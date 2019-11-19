# import from ./data/fbref/2019/
EPLMatches_raw <- readr::read_csv("./data/fbref/2019/EPLMatches.txt",comment="##") # https://fbref.com/en/comps/9/schedule/Premier-League-Fixtures
# EPLPassing_raw <- readr::read_csv("./data/fbref/2019/EPLPassing.txt",comment="##") # 
EPLPlayerShooting_raw <- readr::read_csv("./data/fbref/2019/EPLPlayerShooting.txt",comment="##") #
EPLPlayerStats_raw <- readr::read_csv("./data/fbref/2019/EPLPlayerStats.txt",comment="##",skip=1) # https://fbref.com/en/comps/9/stats/Premier-League-Stats
EPLTable_raw <- readr::read_csv("./data/fbref/2019/EPLTable.txt",comment="##") # https://fbref.com/en/comps/9/Premier-League-Stats
EPLTeamShooting_raw <- readr::read_csv("./data/fbref/2019/EPLTeamShooting.txt",comment="##") #
EPLTeamStats_raw <- readr::read_csv("./data/fbref/2019/EPLTeamStats.txt",comment="##",skip=1) # https://fbref.com/en/comps/9/stats/Premier-League-Stats
SFCMatches_raw <- readr::read_csv("./data/fbref/2019/SFCMatches.txt",comment="##") # https://fbref.com/en/squads/33c895d4/Southampton