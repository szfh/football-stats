table_url <- "https://fbref.com/en/comps/9/Premier-League-Stats"

table_tibble <-
  table_url %>%
  read_html() %>%
  html_nodes("#results32321_overall .center , #results32321_overall .right, #results32321_overall .left, #results32321_overall .right") %>%
  html_text %>%
  matrix(nrow = 21, byrow = T) %>%
  as_tibble()

table_raw <- table_tibble %>%
  set_colnames(tableepl_tibble[1,]) %>%
  slice(-1)

table_tibble %>% View("tibble")
table_raw %>% View("raw")
# rm(tableepl_tibble,tableepl_raw)

write.csv(tableepl_raw,file="data/fbref/table_raw.csv",row.names=F)