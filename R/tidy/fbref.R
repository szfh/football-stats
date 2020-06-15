fbref_static <- readRDS(file=here("data","fbref-raw-static.rds"))
fbref_dynamic <- readRDS(file=here("data","fbref-raw-dynamic.rds"))

fbref <- bind_rows(fbref_static,fbref_dynamic)

fbref %<>% 
  select(-any_of(c("statselector","seasoncode","page_url","content_selector_id"))) %>%
  mutate(data=map2(data, page, fbref_clean_names)) %>% # add column titles, remove non-data rows, refactor
  mutate(data=pmap(list(data, page, stattype), fbref_tidy)) # remove cols not required in data

saveRDS(fbref,file=here("data","fbref-tidy.rds"))
rm(fbref_static,fbref_dynamic,fbref)
