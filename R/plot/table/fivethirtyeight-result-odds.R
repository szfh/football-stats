fivethirtyeight_result_odds <- function(season="2022",league="EPL"){
  data <-
    read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv") %>%
    as_tibble(.name_repair = "unique")
  
  league <- league %>%
    str_replace("EPL","Barclays Premier League") %>%
    str_replace("WSL","FA Women's Super League") %>%
    str_replace("UCL", "UEFA Champions League") %>%
    str_replace("UEL","UEFA Europa League") %>%
    str_replace("UECL","UEFA Europa Conference League") %>%
    str_replace("EFLC", "English League Championship") %>%
    str_replace("EFL1", "English League One") %>%
    str_replace("EFL2", "English League Two")
  
  match_odds <-
    data %>%
    filter(season %in% !!(as.numeric(season))) %>%
    filter(league %in% !!league) %>%
    rowwise %>%
    mutate(result_odds=case_when(
      score1>score2 ~ prob1,
      score1<score2 ~ prob2,
      score1==score2 ~ probtie+min(prob1,prob2),
      TRUE ~ 1
    )) %>%
    ungroup() %>%
    filter(result_odds!=1)
  
  match_odds %>%
    mutate(match=glue("{team1} **{score1}-{score2}** {team2}")) %>%
    select(match,result_odds) %>%
    arrange(result_odds) %>%
    slice_min(result_odds,n=12) %>%
    gt() %>%
    fmt_markdown(columns = match, rows=everything()) %>%
    fmt_percent(columns = result_odds, rows=everything(),decimals=1) %>%
    tab_header(
      title = md("**Results by pre-match odds**")) %>%
    tab_options(
      heading.subtitle.font.size = 12,
      heading.align = "center",
      table.border.top.color = "black",
      heading.border.bottom.color = "black",
      column_labels.border.top.color = "black",
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width= px(3)
    ) %>%
    cols_label(
      # season=md("**Season**"),
      match=md("**Match**"),
      result_odds=md("**Pre-match odds**"),
    ) %>%
    tab_style(
      style = list(
        cell_text(weight="bold")
      ),
      locations = cells_body(columns=c(result_odds))
    ) %>%
    data_color(
      columns=c(result_odds),
      colors=scales::col_numeric(
        palette=as.character(paletteer::paletteer_d(palette="ggsci::red_material",direction = -1)),
        alpha=0.5,
        domain=NULL
      )
    ) %>%
    cols_align(
      align = "center",
      columns = everything()) %>%
    tab_source_note(
      md("[ Women's Super League since 2017 | fivethirtyeight pre-match odds | W-D-L result ]"))
}
