Southampton FC Transfermarkt Scraper
================
saintsnumbers
2019-07-28

## Scraping Southampton FC Transfermarkt data

### Plan

  - Load the webpage containing the data.
  - Locate the data within the page and extract it.
  - Organise the data into a dataframe.
  - Load more data.
  - Do stats on it.

### Load the webpage

  - [**Southampton FC Transfermarkt**
    (detailed)](https://www.transfermarkt.co.uk/southampton-fc/kader/verein/180/saison_id/2019/plus/1)
  - [**Southampton FC Transfermarkt** Premier League 18/19
    (detailed)](https://www.transfermarkt.co.uk/southampton-fc/leistungsdaten/verein/180/reldata/GB1%262018/plus/1)
  - [Page used in the
    example](https://www.transfermarkt.co.uk/manchester-city/startseite/verein/281/saison_id/2019)

<!-- end list -->

``` r
squad_url <- "https://www.transfermarkt.co.uk/southampton-fc/kader/verein/180/saison_id/2019/plus/1"
season19_url <- "https://www.transfermarkt.co.uk/southampton-fc/leistungsdaten/verein/180/reldata/GB1%262018/plus/1"
squad_scraped <- read_html("https://www.transfermarkt.co.uk/southampton-fc/kader/verein/180/saison_id/2019/plus/1")
season19_scraped <- read_html("https://www.transfermarkt.co.uk/southampton-fc/leistungsdaten/verein/180/reldata/GB1%262018/plus/1")
```

### Extract the data

| Done         | Needs tidying | Not done    |
| ------------ | ------------- | ----------- |
| Name         | Height        |             |
| Squad Number | Foot          | 18/19 Stats |
| Position     |               |             |
| DOB/Age      |               |             |
| Contract     |               |             |
| Joined       |               |             |

``` r
player_names_raw <- squad_scraped %>%
  html_nodes(".spielprofil_tooltip") %>%
  html_text()

player_names <- player_names_raw[seq(1,length(player_names_raw),2)] #duplicated - take odd numbered data only

player_numbers <- squad_scraped %>%
  html_nodes(".rn_nummer") %>%
  html_text
player_numbers[player_numbers == "-"] <- NA

player_position <- squad_scraped %>%
  html_nodes(".inline-table tr:nth-child(2) td") %>%
  html_text

player_dob <- squad_scraped %>%
  html_nodes("#yw1 td:nth-child(3)") %>%
  html_text

player_height <- squad_scraped %>%
  html_nodes("td:nth-child(5)") %>%
  html_text

player_foot <- squad_scraped %>%
  html_nodes("td:nth-child(6)") %>%
  html_text

player_joined <- squad_scraped %>%
  html_nodes("td:nth-child(7)") %>%
  html_text

player_contract <- squad_scraped %>%
  html_nodes("td:nth-child(9)") %>%
  html_text
```

### Tidy data

``` r
player_data <- data.frame(Name = player_names, Number = player_numbers, Position = player_position, DoB = player_dob, Height = player_height, Foot = player_foot, Joined = player_joined, Contract = player_contract)

player_data %<>%
  as_tibble

player_data %>%
  head(8)
```

    ## # A tibble: 8 x 8
    ##   Name         Number Position   DoB         Height Foot  Joined   Contract
    ##   <fct>        <fct>  <fct>      <fct>       <fct>  <fct> <fct>    <fct>   
    ## 1 Angus Gunn   28     Goalkeeper Jan 22, 19~ 1,96 m right Jul 10,~ 30.06.2~
    ## 2 Fraser Fors~ 44     Goalkeeper Mar 17, 19~ 2,01 m right Aug 9, ~ 30.06.2~
    ## 3 Alex McCart~ 1      Goalkeeper Dec 3, 198~ 1,93 m right Aug 1, ~ 30.06.2~
    ## 4 Jannik Vest~ 4      Centre-Ba~ Aug 3, 199~ 1,99 m both  Jul 13,~ 30.06.2~
    ## 5 Jan Bednarek 35     Centre-Ba~ Apr 12, 19~ 1,89 m right Jul 1, ~ 30.06.2~
    ## 6 Wesley Hoedt 6      Centre-Ba~ Mar 6, 199~ 1,88 m left  Aug 22,~ 30.06.2~
    ## 7 Jack Stephe~ 5      Centre-Ba~ Jan 27, 19~ 1,85 m right Jul 1, ~ 30.06.2~
    ## 8 Maya Yoshida 3      Centre-Ba~ Aug 24, 19~ 1,89 m right Aug 30,~ 30.06.2~

### To do

  - Get better html selectors process to avoid having to remove
    duplicated data
  - Improve format of data as in table above
  - Scrape new web pages

<!-- end list -->

``` r
# ggplot(player_data) + geom_point(aes(x=Name,y=as.integer(Number)))
```
