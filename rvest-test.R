library(tidyverse)
library(rvest)
library(magrittr)
library(stringr)
# library(rebus)
# library(lubridate)
theme_set(theme_bw())
rm(list=ls())

# https://blog.rstudio.com/2014/11/24/rvest-easy-web-scraping-with-r/
# https://cran.r-project.org/web/packages/rvest/rvest.pdf

# lego_movie_url <- "https://www.imdb.com/title/tt1490017/"
# lego_movie <- read_html(lego_movie_url)

# lego_movie %>%
#   html_node("strong span") %>%
#   html_text %>%
#   as.numeric()

# regex special characters
# https://www.regular-expressions.info/characters.html
# lego_movie %>%
#   html_nodes("td:nth-child(2) a") %>%
#   html_text() %>%
#   str_replace_all("\n","")

# lego_movie %>%
  # html_nodes("table")
# To get to the data, you will need some functions of the rvest package. To convert a website into an XML object, you use the read_html() function. You need to supply a target URL and the function calls the webserver, collects the data, and parses it. To extract the relevant nodes from the XML object you use html_nodes(), whose argument is the class descriptor, prepended by a . to signify that it is a class. The output will be a list of all the nodes found in that way. To extract the tagged data, you need to apply html_text() to the nodes you want. For the cases where you need to extract the attributes instead, you apply html_attrs(). This will return a list of the attributes, which you can subset to get to the attribute you want to extract.
# amazon_url <- "http://www.trustpilot.com/review/www.amazon.com"

query <- "data science"
loc <- "New York"
url_indeed <- "https://www.indeed.com"
session <- html_session(url_indeed)
form <- html_form(session)[[1]]
# form <- set_values(form, q = query, l = loc)

html_form(read_html("https://hadley.wufoo.com/forms/libraryrequire-quiz/"))
html_form(read_html("https://hadley.wufoo.com/forms/r-journal-submission/"))
understatform <- html_form(read_html("https://understat.com/team/Southampton/"))