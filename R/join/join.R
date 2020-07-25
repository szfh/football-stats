fbref <- readRDS(file=here("data","fbref-raw.rds"))

table <- fbref %>%
  filter(page=="league") %>%
  select(-page,-stattype) %>%
  unnest(cols=data)

squad <- fbref %>%
  filter(page=="squad") %>%
  select(-page) %>%
  unnest(cols=data) %>%
  group_by(stattype) %>%
  nest() %>%
  mutate(data=map(data,remove_empty,which="cols")) %$%
  data %>%
  reduce(full_join)

squad <- table %>% left_join(squad)

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
