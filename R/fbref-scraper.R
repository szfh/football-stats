# htmlepl <- "https://fbref.com/en/comps/9/Premier-League-Stats" %>% read_html

tableepl_tibble <-
  htmlepl %>%
  html_nodes("#results32321_overall .center , #results32321_overall .right, #results32321_overall .left, #results32321_overall .right") %>%
  html_text %>%
  matrix(nrow = 21, byrow = T) %>%
  as_tibble()

tableepl_raw <- tableepl_tibble %>%
  set_colnames(tableepl_tibble[1,]) %>%
  slice(-1)

tableepl_tibble %>% View("tibble")
tableepl_raw %>% View("raw")
# rm(tableepl_tibble,tableepl_raw)

write.csv(tableepl_raw,file="data/fbref/tableepl_raw.csv",row.names=F)