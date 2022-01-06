days_since_last_match <- function(team){
  # browser()
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
    # filter(Date!=as.Date("2021-09-26")) %>%
    # add_row( # add wolves game 26/9/21
    #   Comp="Premier League",
    #   Team="Southampton",
    #   Opponent="Wolves",
    #   Date="2021-09-26",
    #   GF="0",
    #   GA="1",
    #   xG=0.8,
    #   xGA=0.9,
    #   Result="L"
  # ) %>%
  filter(Team==!!team) %>%
    mutate(Date=as.Date(Date)) %>%
    # arrange(Date) %>%
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
    filter(Date>=as.Date("2018-12-07")) %>%
    # slice_tail(n=3) %>%
    glimpse
  # view
  
  results <-
    league_fixtures %>%
    group_by(Days_since) %>%
    count(Result) %>%
    ungroup() %>%
    pivot_wider(names_from=Result,values_from=n) %>%
    # mutate(L=ifelse(Days_since==4,L+1,L)) %>% ## add wolves game 26/9/21
    replace_na(replace=list("L"=0,"D"=0,"W"=0)) %>%
    mutate(MP=L+D+W) %>%
    mutate(Pts=(3*W)+D) %>%
    glimpse
  
  goal_diff <-
    league_fixtures %>%
    select(-Poss,-Attendance) %>%
    group_by(Days_since) %>%
    summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
    glimpse
  
  results_by_days <-
    left_join(goal_diff,results) %>%
    relocate(Days_since,MP) %>%
    mutate(across(c(GF:xGD),~./MP)) %>%
    mutate(PPG=Pts/MP) %>%
    glimpse
  
  title <- glue("**{team} 2020-21 + 2021-22**")
  
  table <-
    results_by_days %>%
    mutate(WDL=glue("{W}-{D}-{L}")) %>%
    select(Days_since,P=MP,WDL,GD,xGD,PPG) %>%
    mutate(Days_since=ifelse(Days_since==3,"2-3",as.character(Days_since))) %>%
    mutate(Days_since=ifelse(Days_since==7,"7+",as.character(Days_since))) %>%
    glimpse %>%
    gt() %>%
    # fmt_markdown(data, columns, rows = everything()) %>%
    tab_header(
      title = md(title)) %>%
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
    # cols_align(columns=c(-1),align="center") %>%
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
    ) #%>%
    # data_color(
    #   columns=c(GD,xGD),
    #   colors=scales::col_numeric(
    #     # palette=as.character(paletteer::paletteer_d(palette="ggsci::red_material")),
    #     # palette=as.character(paletteer::paletteer_d(palette="ggsci::red_material")),
    #     # ggsci::rgb_material("red","blue")
    #     # palette=as.character(paletteer::paletteer_d(palette="ggsci::blue_material")),
    #     alpha=0.8,
    #     domain=NULL
    #   )
    # ) %>%
  # gtsave("days_since_last_match.png",here("plots","table")) %>%
  # print
  
  return(table)
}

# {
#   league_fixtures_all <-
#     fixtures %>%
#     group_by(Team) %>%
#     mutate(Date=as.Date(Date)) %>%
#     mutate(Date_previous=lag(Date,1),.after="Date") %>%
#     mutate(Days_since=as.integer(Date-Date_previous),.after="Date_previous") %>%
#     # mutate(across(c(GF,GA,xG,xGA),as.numeric)) %>%
#     mutate(across(c(GF,GA),as.numeric)) %>%
#     mutate(GD=GF-GA) %>%
#     # mutate(xGD=xG-xGA) %>%
#     mutate(Days_since=case_when(
#       is.na(Days_since) ~ 7,
#       Days_since<3 ~ 3,
#       Days_since>7 ~ 7,
#       TRUE ~ as.double(Days_since))) %>%
#     filter(Comp=="Premier League") %>%
#     # filter(Comp=="Championship") %>%
#     glimpse
#   
#   results_all <- 
#     league_fixtures_all %>%
#     group_by(Team,Days_since) %>%
#     count(Result) %>%
#     ungroup() %>%
#     pivot_wider(names_from=Result,values_from=n) %>%
#     replace_na(replace=list("L"=0,"D"=0,"W"=0)) %>%
#     mutate(MP=L+D+W) %>%
#     mutate(Pts=(3*W)+D) %>%
#     glimpse
#   
#   goal_diff_all <-
#     league_fixtures_all %>%
#     select(-Poss,-Attendance) %>%
#     group_by(Team,Days_since) %>%
#     summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
#     glimpse
#   
#   results_by_days_all <-
#     left_join(goal_diff_all,results_all) %>% 
#     relocate(Team,Days_since,MP) %>%
#     # mutate(across(c(GF:xGD),~./MP)) %>%
#     mutate(across(c(GF:GD),~./MP)) %>%
#     mutate(PPG=Pts/MP) %>%
#     glimpse
# }
# 
# {
#   plots <- list()
#   
#   plots$ppg_rest_days <-
#     results_by_days_all %>%
#     mutate(Team=case_when(
#       # Team=="Brighton and Hove Albion" ~ "Brighton",
#       # Team=="Leeds United" ~ "Leeds Utd",
#       # Team=="Manchester United" ~ "Manchester Utd",
#       # Team=="Newcastle United" ~ "Newcastle Utd",
#       # Team=="Sheffield United" ~ "Sheffield Utd",
#       # Team=="Tottenham Hotspur" ~ "Tottenham",
#       # Team=="West Bromwich Albion" ~ "West Brom",
#       # Team=="West Ham United" ~ "West Ham Utd",
#       Team=="Wolverhampton Wanderers" ~ "Wolves",
#       TRUE ~ Team)) %>%
#     mutate(MP_label=glue("{MP}")) %>%
#     mutate(MP_label_y=ifelse(PPG<=2,PPG+0.7,PPG-0.7)) %>%
#     glimpse %>%
#     ggplot(aes(x=Days_since,y=PPG)) +
#     geom_path() +
#     geom_point(colour="darkred",shape="x") +
#     geom_text(aes(label=MP_label,y=MP_label_y),size=1.8) +
#     theme[["solar"]]() +
#     labs(
#       title="Premier League points per game 2020-21",
#       x="Days since last match (labels = matches played)",
#       y=element_blank()) +
#     theme(
#       axis.text.x=element_text(size=6),
#       axis.text.y=element_text(size=6),
#       strip.text=element_text(size=6),
#       axis.line.x.bottom=element_blank()
#     ) +
#     scale_x_continuous(labels=c("2-3","4","5","6","7+"),expand=expansion(add=c(0.2))) +
#     scale_y_continuous(limits=c(0,3),expand=expansion(add=c(0.1,0.1))) +
#     # facet_wrap("Team")
#     facet_wrap("Team",nrow=4)
#   
#   # plots_logo <-
#   #   plots %>%
#   #   add_logo(path=here("images","SB_Regular.png"),x=1,y=1,hjust=1.1,width=0.2) %>%
#   #   add_logo(path=here("images","fbref.png"),x=0.88,y=1,hjust=1.1,width=0.29)
#   
#   # save_plots(plots,path=here("plots","test"))
# }
