require(tidyverse)
require(magrittr)
require(rvest)
# more info about acciotables: https://github.com/npranav10/acciotables/

season <- 10728
# this is the key for the 2020-21 EPL season, for others swap "10728" for...
# 2020-21 = 10728
# 2019-20 = 3232
# 2018-19 = 1889
# 2017-18 = 1631

# get all the fixtures from the season
url_schedule <- paste0("https://acciotables.herokuapp.com/?page_url=https://fbref.com/en/comps/9/",season,"/schedule/&content_selector_id=%23sched_ks_",season,"_1")

# read html from API
data <-
  url_schedule %>%
  read_html()

#get match codes
match_codes <-
  data %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  as_tibble() %>%
  separate(value,c(NA,NA,"datatype","code","desc"),sep="/",extra="merge",fill="right") %>%
  filter(datatype=="matches") %>%
  filter(!is.na(desc)) %>%
  # select("code") %>%
  extract2("code") %>%
  unique()

# make URLs for each shot table
url_shots <- paste0("https://acciotables.herokuapp.com/?page_url=https://fbref.com/en/matches/",match_codes,"/&content_selector_id=%23shots_all")

# function to get each shot table using map
get_shots <- function(url_shots){
  print(url_shots) # to check it's still alive

  shots <-
    url_shots %>%
    read_html() %>%
    html_table(header=FALSE) %>%
    extract2(1) %>%
    as_tibble()
  
  # the column names are in the first two rows
  names(shots) <-
    paste(shots[1,],shots[2,]) %>%
    str_squish()
  
  # delete the first two rows and any non-data rows
  shots <-
    shots %>%
    slice(-1,-2) %>%
    filter(Player!="Player") %>%
    filter(Player!="")
  
  return(shots)
}

# loop through the URLs and write the shots table to a data.frame
# this takes a while (10+ minutes for a full season), so go and make a cup of tea
shots <- map_dfr(url_shots[1:5],possibly(get_shots,otherwise=NULL))
# note: the [1:5] takes the first five matches to check the code works properly
# when you're happy, delete that bit and run for real to get all the data

# add your own file path and uncomment this to save a .csv file
# write_csv(shots,"C:\\Users\\<username>\\Desktop\\EPL_shot_data.csv")
