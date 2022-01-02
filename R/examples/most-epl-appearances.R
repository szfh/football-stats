library(here)
library(tidyverse)
library(gt)

jwp_games_2022 <- 6

most_matches <-
  tribble(
    ~" ",~"Player",~"Apps",~"Team",
    1,"Wayne Bridge",112,"Southampton",
    2,"Leighton Baines",99,"Everton",
    3,"Conor Coady",84,"Wolves",
    4,"James Ward-Prowse",1+38+38+jwp_games_2022,"Southampton",
    5,"Alan Wright",82,"Aston Villa",
    6,"Jamie Carragher",76,"Liverpool",
    6,"Harry Maguire",76,"Leicester City<br>Manchester United",
    8,"Wes Morgan",75,"Leicester City",
    9,"Matt Holland",74,"Ipswich Town",
    9,"César Azpilicueta",74,"Chelsea",
    9,"Virgil van Dijk",74,"Liverpool",
    12,"Gareth Southgate",71,"Crystal Palace<br>Aston Villa",
    12,"Gary Pallister",71,"Manchester United",
  ) %>%
  mutate(Mins=Apps*90) %>%
  relocate(Mins,.after=Apps)

most_matches %>% 
  gt() %>%
  fmt_markdown(columns = everything()) %>%
  tab_header(
    title = md("**Unbroken Premier League appearances**")) %>%
  tab_options(
    heading.subtitle.font.size = 12,
    table.border.top.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
  ) %>%
  tab_style(
    style = cell_text(
      font = google_font("Open Sans")
    ),
    locations = list(
      cells_title("title"),
      cells_column_labels(),
      cells_body(),
      cells_footnotes()
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color="darkred", alpha=0.3),
      cell_text(style="italic")
    ),
    locations = cells_body(
      rows=4
    )
  ) %>%
  cols_align(align = "left",
             columns = 3) %>% 
  tab_source_note(
    md("**consecutive matches without missing a minute | outfield players only**")) %>%
  gtsave("most-epl-appearances.png",here("plots","table")) %>%
  print
