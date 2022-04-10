data$fbref$advanced_stats_player_summary %>%
  arrange(npxG_Expected) %>%
  slice_max(npxG_Expected,n=50) %>%
  mutate(Home_Team=shorten_team_names(Home_Team)) %>%
  mutate(Away_Team=shorten_team_names(Away_Team)) %>%
  mutate(Season=glue("{str_sub(Season,start=3,end=4)}-{str_sub(Season,start=8,end=9)}")) %>%
  mutate(Match=glue("{Home_Team} **{Home_Score}-{Away_Score}** {Away_Team}")) %>%
  select(Season,Match,Player,xG=npxG_Expected,Goals=Gls,Shots=Sh,Mins=Min) %>%
  slice_max(xG,n=12) %>%
  gt() %>%
  fmt_markdown(columns = Match, rows=everything()) %>%
  tab_header(
    title=md("Highest single-match expected goals")) %>%
  tab_options(
    heading.subtitle.font.size = 12,
    heading.align = "center",
    table.border.top.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width= px(3)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color="darkgrey", alpha=0.6),
      cell_text(style="italic")
    ),
    locations = cells_body(
      columns=everything(),rows=1
    )
  ) %>%
  data_color(
    columns=c(Goals,xG),
    colors=scales::col_numeric(
      palette=as.character(paletteer::paletteer_d(palette="ggsci::blue_material",direction = 1)),
      alpha=0.4,
      domain=NULL
    )
  ) %>%
  cols_align(
    align = "center",
    columns = everything()) %>%
  tab_source_note(
    md("[ Statsbomb / FBRef data | PL non-penalty expected goals since 17-18 ]")) %>%
  gtsave(filename=glue("most_xg.png"),path=here("plots","test"))
