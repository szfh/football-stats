raw <- list()

raw[["fivethirtyeight"]][["matches"]] <- read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv") %>%
  as_tibble(.name_repair = "unique")
raw[["fivethirtyeight"]][["rankings"]] <- read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_global_rankings.csv") %>%
  as_tibble(.name_repair = "unique")

saveRDS(raw,file=here("data","raw-fivethirtyeight.rds"))
rm(raw)
