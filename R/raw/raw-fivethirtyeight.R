scrape_fivethirtyeight <- function(save_path=here("data","fivethirtyeight.rds")){
  fivethirtyeight <-
    read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv") %>%
    as_tibble(.name_repair = "unique")

  saveRDS(fivethirtyeight,file=save_path)
  
  return(fivethirtyeight)
}
