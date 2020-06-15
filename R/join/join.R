fbref <- readRDS(file=here("data","fbref-tidy.rds"))

squad <- fbref %>%
  filter(page=="squad") %>%
  select(-page) %>%
  unnest(cols=data) %>%
  group_by(stattype) %>%
  nest() %>%
  mutate(data=map(data,remove_empty,which="cols")) %$%
  data %>%
  reduce(full_join) %>%
  print

players <- fbref %>%
  filter(page=="player") %>%
  select(-page) %>%
  unnest(cols=data) %>%
  group_by(stattype) %>%
  nest() %>%
  mutate(data=map(data,remove_empty,which="cols")) %$%
  data %>%
  reduce(full_join)

matches <- fbref %>%
  filter(page=="schedule") %>%
  select(-page,-stattype) %>%
  unnest(cols=data)
