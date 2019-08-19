library(tidyverse)
library(rvest)
library(magrittr)
library(stringr)
theme_set(theme_bw())
rm(list=ls())

# https://blog.rstudio.com/2014/11/24/rvest-easy-web-scraping-with-r/
# https://cran.r-project.org/web/packages/rvest/rvest.pdf
# https://www.datacamp.com/community/tutorials/r-web-scraping-rvest

lego_movie_url <- "https://www.imdb.com/title/tt1490017/"
lego_movie <- read_html(lego_movie_url)

lego_movie %>%
  html_node("strong span") %>%
  html_text %>%
  as.numeric()

lego_movie %>%
  html_nodes("td:nth-child(2) a") %>%
  html_text() %>%
  str_replace_all("\n","")
  # str_replace_all("\\\","")