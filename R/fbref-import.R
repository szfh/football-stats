path <- "./data/fbref/EPL/2019/"
csv_comment <- "##"

# read_csv(glue(path,"Standard_Player"),skip=1,comment=csv_comment)

Standard_Player_raw <- read_csv(glue(path,"Standard_Player.txt"),skip=2,comment=csv_comment)
Standard_Team_raw <- read_csv(glue(path,"Standard_Team.txt"),skip=2,comment=csv_comment)

Passing_Player_raw <- read_csv(glue(path,"Passing_Player.txt"),skip=2,comment=csv_comment)
Passing_Team_raw <- read_csv(glue(path,"Passing_Team.txt"),skip=2,comment=csv_comment)

Shooting_Player_raw <- read_csv(glue(path,"Shooting_Player.txt"),skip=1,comment=csv_comment)
Shooting_Team_raw <- read_csv(glue(path,"Shooting_Team.txt"),skip=1,comment=csv_comment)

Misc_Player_raw <- read_csv(glue(path,"Misc_Player.txt"),skip=0,comment=csv_comment)
Misc_Team_raw <- read_csv(glue(path,"Misc_Team.txt"),skip=0,comment=csv_comment)

PlayingTime_Player_raw <- read_csv(glue(path,"PlayingTime_Player.txt"),skip=2,comment=csv_comment)
PlayingTime_Team_raw <- read_csv(glue(path,"PlayingTime_Team.txt"),skip=2,comment=csv_comment)

Table_raw <- read_csv(glue(path,"Table.txt"),skip=0,comment=csv_comment)
Matches_raw <- read_csv(glue(path,"Matches.txt"),skip=0,comment=csv_comment)