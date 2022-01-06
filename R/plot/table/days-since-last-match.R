days_since_last_match <- function(team){
  
  # data <- join(save_path_fbref=here("data","fbref.rds"))
  # team_urls1819 <- worldfootballR::fb_teams_urls("https://fbref.com/en/comps/9/1889/")
  # team_urls1920 <- worldfootballR::fb_teams_urls("https://fbref.com/en/comps/9/3232/")
  team_urls2021 <- worldfootballR::fb_teams_urls("https://fbref.com/en/comps/9/10728/")
  team_urls2122 <- worldfootballR::fb_teams_urls("https://fbref.com/en/comps/9/")
  # team_urls <- worldfootballR::fb_teams_urls("https://fbref.com/en/comps/10/")
  # team_urls <- worldfootballR::fb_teams_urls("https://fbref.com/en/comps/15/")
  # team_urls <- worldfootballR::fb_teams_urls("https://fbref.com/en/comps/16/")
  # fixtures1819 <- worldfootballR::get_team_match_results(team_urls1819)
  # fixtures1920 <- worldfootballR::get_team_match_results(team_urls1920)
  fixtures2021 <- worldfootballR::get_team_match_results(team_urls2021)
  fixtures2122 <- worldfootballR::get_team_match_results(team_urls2122)
  fixtures <- bind_rows(fixtures2021,fixtures2122)
  # fixtures <- bind_rows(fixtures1819,fixtures1920,fixtures2021,fixtures2122)
  
  league_fixtures <-
    fixtures %>%
    filter(Team==!!team) %>%
    mutate(Date=as.Date(Date)) %>%
    mutate(Date_previous=lag(Date,1),.after="Date") %>%
    mutate(Days_since=as.integer(Date-Date_previous),.after="Date_previous") %>%
    mutate(across(c(GF,GA,xG,xGA),as.numeric)) %>%
    mutate(GD=GF-GA) %>%
    mutate(xGD=xG-xGA) %>%
    mutate(Days_since=case_when(
      is.na(Days_since) ~ 7,
      Days_since<3 ~ 3,
      Days_since>7 ~ 7,
      TRUE ~ as.double(Days_since))) %>%
    filter(Comp=="Premier League") %>%
    filter(!is.na(GF)) %>%
    filter(Date>=as.Date("2018-12-07"))
  
  results <-
    league_fixtures %>%
    group_by(Days_since) %>%
    count(Result) %>%
    ungroup() %>%
    pivot_wider(names_from=Result,values_from=n) %>%
    replace_na(replace=list("L"=0,"D"=0,"W"=0)) %>%
    mutate(MP=L+D+W) %>%
    mutate(Pts=(3*W)+D)
  
  goal_diff <-
    league_fixtures %>%
    select(-Poss,-Attendance) %>%
    group_by(Days_since) %>%
    summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop")
  
  results_by_days <-
    left_join(goal_diff,results) %>%
    relocate(Days_since,MP) %>%
    mutate(across(c(GF:xGD),~./MP)) %>%
    mutate(PPG=Pts/MP)
  
  title <- glue("**{team} 2020-21 + 2021-22**")
  
  table <-
    results_by_days %>%
    mutate(WDL=glue("{W}-{D}-{L}")) %>%
    select(Days_since,P=MP,WDL,GD,xGD,PPG) %>%
    mutate(Days_since=ifelse(Days_since==3,"2-3",as.character(Days_since))) %>%
    mutate(Days_since=ifelse(Days_since==7,"7+",as.character(Days_since))) %>%
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
      Days_since = md("Days since<br>last match"),
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
        cells_column_labels(-"Days_since"),
        cells_body()
      )
    ) %>%
    tab_style(
      style = cell_text(
        font = google_font("Open Sans"),
        size = "small"
      ),
      locations = list(cells_column_labels("Days_since"))
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
