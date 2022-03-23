scrape_fivethirtyeight <- function(save_path=here("data","fivethirtyeight.rds")){
  fivethirtyeight <- list()
  
  fivethirtyeight$matches <-
    read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv") %>%
    as_tibble(.name_repair = "unique")
  
  fivethirtyeight$matches_latest <-
    read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches_latest.csv") %>%
    as_tibble(.name_repair = "unique")
  
  fivethirtyeight$global_rankings <-
    read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_global_rankings.csv") %>%
    as_tibble(.name_repair = "unique")
  
  fivethirtyeight$global_rankings_intl <-
    read.csv("https://projects.fivethirtyeight.com/soccer-api/international/spi_global_rankings_intl.csv") %>%
    as_tibble(.name_repair = "unique")
  
  saveRDS(fivethirtyeight,file=save_path)
  
  return(fivethirtyeight)
}
