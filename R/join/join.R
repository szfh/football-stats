source(here("R","tidy","tidy-utils.R"))
# fbref <- readRDS(file=here("data","fbref-raw.rds"))
# understat <- readRDS(file=here("data","understat-raw.rds"))

join <- function(){
  fbref <- readRDS(file=here("data","fbref-raw.rds"))
  understat <- readRDS(file=here("data","understat-raw.rds"))
  
  # tidy  
  fbref <- fbref %>%
    mutate(data=pmap(list(data,page,stattype), possibly(fbref_tidy, otherwise=NA))) %>%
    select(-any_of(c("statselector","seasoncode","page_url","content_selector_id")))
  
  understat <- understat
  # mutate(data=pmap(function_here)) %>% # make understat_tidy?
  # select() # delete non-required columns
  
  # join
  table <-
    fbref %>%
    filter(page=="league") %>%
    select(-page,-stattype) %>%
    unnest(cols=data)

  squad <-
    fbref %>%
    filter(page=="squad") %>%
    select(-page) %>%
    unnest(cols=data) %>%
    group_by(stattype) %>%
    nest() %>%
    mutate(data=map(data,remove_empty,which="cols")) %$%
    data %>%
    reduce(full_join)

  squad <-
    table %>%
    left_join(squad)

  players <-
    fbref %>%
    filter(page=="player") %>%
    select(-page) %>%
    unnest(cols=data) %>%
    group_by(stattype) %>%
    nest() %>%
    mutate(data=map(data,remove_empty,which="cols")) %$%
    data %>%
    reduce(full_join)

  matches <-
    fbref %>%
    filter(page=="schedule") %>%
    select(-page,-stattype) %>%
    unnest(cols=data)

  shots <-
    understat %>%
    filter(datatype=="shots") %>%
    select(data) %>%
    unnest(data)

  match_stats <-
    understat %>%
    filter(datatype=="stats") %>%
    select(data) %>%
    unnest(data)
  
  browser()
  data <- list("table"=table,"squad"=squad,"players"=players,"matches"=matches,"shots"=shots,"match_stats"=match_stats)
  return(data)
}
