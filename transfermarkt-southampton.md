Southampton FC Transfermarkt Scraper
================
saintsnumbers
2019-07-28

## Scraping Southampton FC Transfermarkt data.

### Plan

  - Load the webpage containing the data.
  - Locate the data within the page and extract it.
  - Organise the data into a dataframe

### Load the webpage

  - [**Southampton FC Transfermarkt**
    (detailed)](https://www.transfermarkt.co.uk/southampton-fc/kader/verein/180/saison_id/2019/plus/1)
  - [Page used in the
    example](https://www.transfermarkt.co.uk/manchester-city/startseite/verein/281/saison_id/2019)

<!-- end list -->

``` r
page_url <- "https://www.transfermarkt.co.uk/southampton-fc/kader/verein/180/saison_id/2019/plus/1"
page_scraped <- read_html(page_url)
```

### Extract the data

| Done         | Needs tidying | Not done |
| ------------ | ------------- | -------- |
| Name         | Height        | DOB/Age  |
| Squad Number | Foot          | Contract |
| Position     |               | Joined   |

``` r
player_names_raw <- page_scraped %>%
  html_nodes(".spielprofil_tooltip") %>%
  html_text()

player_names <- player_names_raw[seq(1,length(player_names_raw),2)] #duplicated - odd numbered data only

player_numbers <- page_scraped %>%
  html_nodes(".rn_nummer") %>%
  html_text

player_position <- page_scraped %>%
  html_nodes(".inline-table tr:nth-child(2) td") %>%
  html_text

player_height <- page_scraped %>%
  html_nodes("td:nth-child(5)") %>%
  html_text

player_foot <- page_scraped %>%
  html_nodes("td:nth-child(6)") %>%
  html_text

player_name_alt <- page_scraped %>%
  html_nodes(".posrela") %>%
  html_text
```

### Tidy data

``` r
player_data <- data.frame(Name = player_names, No = player_numbers, Position = player_position, Height = player_height, Foot = player_foot)

player_data %<>%
  as_tibble %>%
  mutate(No = as.integer(No))
  
player_data
```

    ## # A tibble: 30 x 5
    ##    Name                  No Position    Height Foot 
    ##    <fct>              <int> <fct>       <fct>  <fct>
    ##  1 Angus Gunn            15 Goalkeeper  1,96 m right
    ##  2 Fraser Forster        22 Goalkeeper  2,01 m right
    ##  3 Alex McCarthy          2 Goalkeeper  1,93 m right
    ##  4 Jannik Vestergaard    19 Centre-Back 1,99 m both 
    ##  5 Jan Bednarek          17 Centre-Back 1,89 m right
    ##  6 Wesley Hoedt          24 Centre-Back 1,88 m left 
    ##  7 Jack Stephens         23 Centre-Back 1,85 m right
    ##  8 Maya Yoshida          16 Centre-Back 1,89 m right
    ##  9 Ryan Bertrand         12 Left-Back   1,79 m left 
    ## 10 Sam McQueen            5 Left-Back   1,81 m left 
    ## # ... with 20 more rows

### To do

  - Turn into proper tibble
      - Started
  - Try to get better html selectors process to avoid having to remove
    duplicated data
  - Improve format of data as in table above
