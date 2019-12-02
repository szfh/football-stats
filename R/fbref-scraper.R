# htmlbb <- "https://www.basketball-reference.com/players/j/jamesle01.html" %>% read_html()

# htmlsfc <- "https://fbref.com/en/squads/33c895d4/Southampton" %>% read_html()

# htmlepl <- "https://fbref.com/en/comps/9/Premier-League-Stats" %>% read_html

htmlbb %>%
  html_nodes(".full_table .right , .full_table .center , #per_game .full_table .left") %>% 
  html_text %>% 
  matrix(ncol = 30, byrow = T) %>% 
  as.data.frame %>%
  View()

tableepl <- htmlepl %>%
  html_nodes("#results32321_overall .center , #results32321_overall .right, #results32321_overall .left, #results32321_overall .right") %>%
  html_text %>%
  matrix(nrow = 21, byrow = T) %>%
  as.data.frame()
# as_tibble
