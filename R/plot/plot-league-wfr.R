source(here("R","plot","plot-utils.R"),encoding="utf-8")
source(here("R","themes.R"),encoding="utf-8")

plot_league_wfr <- function(data,season="2020-2021"){
  
  force(data)
  plots <- list()
  season <- expand_seasons(season)
  
  plots$goals_xg <-
    data$fbref$advanced_stats_player_summary %>%
    filter(Season %in% !!season) %>%
    select(Player,Team,Gls,PK,npxG=npxG_Expected) %>%
    mutate(npG=Gls-PK,.keep="unused",.after="PK") %>%
    group_by(Player,Team) %>%
    summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
    make_long_data(levels=c("npG","npxG"),labels=c("Goals","Expected Goals")) %>%
    mutate(focus=case_when(
      key=="Goals" & min_rank(desc(n))<=10 ~ TRUE,
      key=="Expected Goals" & min_rank(desc(n))<=20 ~ TRUE,
      TRUE ~ FALSE)) %>%
    ggplot(aes(x=0.05,y=n,alpha=focus)) +
    geom_text_repel(
      aes(label=ifelse(focus,Player,"")),
      size=2.5,
      nudge_x=0.3,
      direction="y",
      hjust=0,
      segment.size=0.4,
      box.padding=0.05
    ) +
    geom_point(aes(fill=Team),shape=23,size=3,position=position_jitterdodge(jitter.width=0,jitter.height=0,dodge.width=0.04,seed=2)) +
    theme[["solarfacet"]]() +
    facet_wrap("key",scales="free") +
    labs(
      title="Expected Goals (penalties excluded)",
      x=element_blank(),
      y=element_blank()) +
    scale_x_continuous(limit=c(0,1)) +
    scale_y_continuous() +
    scale_fill_manual(values=palette[["epl"]]()) +
    scale_alpha_manual(values=c("TRUE"=1,"FALSE"=0.1))
  
  penalties <-
    data$fbref$advanced_stats_team_summary %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    filter(!is.na(PKatt)) %>%
    select(Match_Date,Home_Team,Away_Team,Home_Away,PKatt) %>%
    pivot_wider(names_from=Home_Away, values_from=PKatt, names_glue="PK_{Home_Away}")
  
  plots$xg_for_against <-
    data$fbref$advanced_stats_team_summary %>%
    filter(Season %in% !!season) %>%
    filter((!is.na(Home_xG))|!is.na(Away_xG)) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    select(Match_Date,Home_Team,Away_Team,Home_xG,Away_xG,Team,Home_Away) %>%
    left_join(penalties) %>%
    arrange(Match_Date) %>%
    mutate(
      Home_npxG=Home_xG-(PK_Home*0.7),
      Away_npxG=Away_xG-(PK_Away*0.7)
    ) %>%
    mutate(
      Team_npxG=ifelse(Home_Away=="Home",Home_npxG,Away_npxG),
      Opposition_npxG=ifelse(Home_Away=="Home",-Away_npxG,-Home_npxG)
    ) %>%
    select(Team,Team_npxG,Opposition_npxG) %>%
    group_by(Team) %>%
    summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
    make_long_data(levels=c("Team_npxG","Opposition_npxG"),labels=c("Expected Goals For","Expected Goals Against")) %>%
    ggplot(aes(x=0.05,y=n)) +
    geom_text_repel(
      aes(label=Team),
      size=2.5,
      nudge_x=0.3,
      direction="y",
      hjust=0,
      segment.size=0.4,
      box.padding=0.05
    ) +
    geom_point(aes(fill=Team),shape=23,size=3,position=position_jitterdodge(jitter.width=0,jitter.height=0,dodge.width=0.04,seed=2)) +
    theme[["solarfacet"]]() +
    facet_wrap("key",scales="free") +
    labs(
      title="Expected Goals",
      x=element_blank(),
      y=element_blank()) +
    scale_x_continuous(limit=c(0,1)) +
    scale_y_continuous(breaks=seq(-100,100,5),labels=abs(seq(-100,100,5)),expand=expansion(add=2)) +
    scale_fill_manual(values=palette[["epl"]]())
  
  plots$xg_for_against_sc <-
    data$fbref$advanced_stats_team_summary %>%
    filter(Season %in% !!season) %>%
    mutate(Match_Date=parse_date_time(Match_Date,"mdy")) %>%
    select(Match_Date,Home_Team,Away_Team,Home_xG,Away_xG,Team,Home_Away) %>%
    left_join(penalties) %>%
    mutate(
      Home_npxG=Home_xG-(PK_Home*0.7),
      Away_npxG=Away_xG-(PK_Away*0.7)
    ) %>%
    mutate(
      Team_npxG=ifelse(Home_Away=="Home",Home_npxG,Away_npxG),
      Opposition_npxG=ifelse(Home_Away=="Home",Away_npxG,Home_npxG)
    ) %>%
    select(Team,Team_npxG,Opposition_npxG) %>%
    group_by(Team) %>%
    summarise(across(where(is.numeric),sum,na.rm=TRUE),.groups="drop") %>%
    ggplot(aes(x=Team_npxG,y=Opposition_npxG)) +
    geom_text_repel(aes(label=Team),size=2) +
    geom_point(aes(fill=Team),shape=23,size=2.5) +
    theme[["solar"]]() +
    labs(
      title="Expected goals",
      x="xG for",
      y="xG against"
    ) +
    scale_x_continuous(breaks=seq(0,100,5),expand=expansion(add=c(2))) +
    scale_y_reverse(breaks=seq(0,100,5),expand=expansion(add=c(2))) +
    scale_fill_manual(values=palette[["epl"]]())
  
  plots_logo <-
    plots %>%
    add_logo(path=here("images","SB_Regular.png"),x=1,y=1,hjust=1.1,width=0.2) %>%
    add_logo(path=here("images","fbref.png"),x=0.88,y=1,hjust=1.1,width=0.29)
  
  save_plots(plots_logo,path=here("plots","league_wfr"))
  
}