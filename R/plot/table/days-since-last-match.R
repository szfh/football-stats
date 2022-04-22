days_since_last_match <- function(team,season){
  
  season <- unique(expand_seasons(season))
  
  fixtures <-
    data$fbref$match_results_allcomp %>%
    filter(Season %in% !!season) %>%
    filter(Team %in% !!team) %>%
    filter(Result!="") %>%
    mutate(Date=as.Date(Date)) %>%
    arrange(Date) %>%
    mutate(Date_last_match=lag(Date,1),.after="Date") %>%
    mutate(Days_between_matches=as.integer(Date-Date_last_match),.after="Date_last_match") %>%
    mutate(Days_between_matches=case_when(
      is.na(Days_between_matches) ~ 7,
      Days_between_matches<3 ~ 3,
      Days_between_matches>7 ~ 7,
      TRUE ~ as.double(Days_between_matches))) %>%
    filter(Comp=="Premier League") %>%
    mutate(across(c(GF,GA,xG,xGA),as.numeric)) %>%
    mutate(GD=GF-GA) %>%
    mutate(xGD=xG-xGA)
  
  results <-
    fixtures %>%
    group_by(Days_between_matches) %>%
    count(Result) %>%
    ungroup() %>%
    pivot_wider(names_from=Result,values_from=n) %>%
    replace_na(replace=list("L"=0,"D"=0,"W"=0)) %>%
    mutate(MP=L+D+W,.after="Days_between_matches") %>%
    mutate(Pts=(3*W)+D)
  
  goal_diff <-
    fixtures %>%
    select(-Season,-Poss,-Attendance) %>%
    group_by(Days_between_matches) %>%
    summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop")
  
  results_by_days <-
    left_join(goal_diff,results) %>%
    relocate(Days_between_matches,MP) %>%
    mutate(across(c(GF:xGD),~./MP)) %>%
    mutate(PPG=Pts/MP)
  
  title <- glue("**{team} {season[1]}**")
  
  table <-
    results_by_days %>%
    mutate(WDL=glue("{W}-{D}-{L}")) %>%
    select(Days_between_matches,P=MP,WDL,GD,xGD,PPG) %>%
    mutate(Days_between_matches=ifelse(Days_between_matches==3,"2-3",as.character(Days_between_matches))) %>%
    mutate(Days_between_matches=ifelse(Days_between_matches==7,"7+",as.character(Days_between_matches))) %>%
    gt() %>%
    tab_header(
      title=md(title)) %>%
    tab_options(
      heading.subtitle.font.size = 12,
      heading.align = "center",
      table.border.top.color = "black",
      heading.border.bottom.color = "black",
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width= px(3)
    ) %>%
    cols_label(
      Days_between_matches = md("Days since<br>last match"),
      WDL = md("W-D-L")
    ) %>%
    fmt_number(columns=c(4:6),decimals=2) %>%
    cols_align(columns=everything(),align="center") %>%
    tab_style(
      style = cell_text(
        font = google_font("Open Sans"),
        size = "large"
      ),
      locations = list(
        cells_title("title"),
        cells_column_labels(-"Days_between_matches"),
        cells_body()
      )
    ) %>%
    tab_style(
      style = cell_text(
        font = google_font("Open Sans"),
        size = "small"
      ),
      locations = list(cells_column_labels("Days_between_matches"))
    ) %>%
    tab_style(
      style = list(
        cell_text(weight="bold")
      ),
      locations = cells_body(columns=c("PPG"))
    ) %>%
    gt_color_rows(
      c("GD","xGD"),
      palette = c("darkred","lightblue"),
      domain=c(-1.8,0.5),
      use_paletteer=FALSE
    ) %>%
    gt_color_rows(
      c("PPG"),
      palette = c("grey","lightgreen"),
      domain=range(results_by_days$PPG),
      use_paletteer=FALSE
    )
  
  return(table)
}
