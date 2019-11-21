# import from ./data/fbref/2019/

EPLMatches_raw <- readr::read_csv("./data/fbref/2019/EPLMatches.txt",skip=1,comment="##")
# https://fbref.com/en/comps/9/schedule/Premier-League-Fixtures

EPLPlayerPassing_raw <- readr::read_csv("./data/fbref/2019/EPLPlayerPassing.txt",skip=2,comment="##")
# https://fbref.com/en/comps/9/passing/Premier-League-Stats

EPLPlayerShooting_raw <- readr::read_csv("./data/fbref/2019/EPLPlayerShooting.txt",skip=1,comment="##")
# https://fbref.com/en/comps/9/shooting/Premier-League-Stats

EPLPlayerStats_raw <- readr::read_csv("./data/fbref/2019/EPLPlayerStats.txt",comment="##",skip=2)
# https://fbref.com/en/comps/9/stats/Premier-League-Stats

EPLTable_raw <- readr::read_csv("./data/fbref/2019/EPLTable.txt",skip=1,comment="##")
# https://fbref.com/en/comps/9/Premier-League-Stats

EPLTeamPassing_raw <- readr::read_csv("./data/fbref/2019/EPLTeamPassing.txt",skip=2,comment="##")
# https://fbref.com/en/comps/9/passing/Premier-League-Stats

EPLTeamShooting_raw <- readr::read_csv("./data/fbref/2019/EPLTeamShooting.txt",comment="##")
# https://fbref.com/en/comps/9/shooting/Premier-League-Stats

EPLTeamStats_raw <- readr::read_csv("./data/fbref/2019/EPLTeamStats.txt",skip=2,comment="##")
# https://fbref.com/en/comps/9/stats/Premier-League-Stats

SFCMatches_raw <- readr::read_csv("./data/fbref/2019/SFCMatches.txt",comment="##")
# https://fbref.com/en/squads/33c895d4/Southampton