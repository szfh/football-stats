expand_seasons <- function(season){
  season <- c(season,str_sub(season,-4))
  # season <- distinct(season)
  return(season)
}

shorten_team_names <- function(team){
  team <- team %>%
    str_replace("United","Utd")
  
  team <- case_when(
    team %in% "Brighton & Hove Albion" ~ "Brighton",
    team %in% "Huddersfield Town" ~ "Huddersfield",
    team %in% "Tottenham Hotspur" ~ "Tottenham",
    team %in% "West Bromwich Albion" ~ "West Brom",
    team %in% "Wolverhampton Wanderers" ~ "Wolves",
    TRUE ~ team
  )
  
  return(team)
}

make_std_date <- function(date){
  new_date <- lubridate::parse_date_time(date,"mdy")
  
  return(new_date)
}

make_long_data <- function(data,levels,labels){ # transform data to long format
  data %<>%
    filter_at(levels,any_vars(!is.na(.))) %>%
    pivot_longer(cols=levels,names_to="key",values_to="n") %>%
    mutate(key=factor(key,levels=levels,labels=labels)) %>%
    group_by(key)
  
  return(data)
}

make_for_against_matches <- function(matches){
  
  matches_for_against <-
    matches %>%
    mutate(Opposition=ifelse(Home_Away=="Home",Away_Team,Home_Team)) %>%
    mutate(Team_Score=ifelse(Home_Away=="Home",Home_Score,Away_Score)) %>%
    mutate(Team_xG=ifelse(Home_Away=="Home",Home_xG,Away_xG)) %>%
    mutate(Team_npxG=ifelse(Home_Away=="Home",Home_npxG,Away_npxG)) %>%
    mutate(Opposition_Score=ifelse(Home_Away=="Home",Away_Score,Home_Score)) %>%
    mutate(Opposition_xG=ifelse(Home_Away=="Home",Away_xG,Home_xG)) %>%
    mutate(Opposition_npxG=ifelse(Home_Away=="Home",Away_npxG,Home_npxG))
  
  return(matches_for_against)
}

make_long_matches <- function(matches){ # transform matches to long format
  
  matches_long <-
    matches %>%
    pivot_longer(cols=c(home,away),
                 names_to="ha",
                 values_to="squad") %>%
    left_join(matches) %>%
    mutate(
      opposition=ifelse(ha=="home",away,home),
      glsf=ifelse(ha=="home",homegls,awaygls),
      glsa=ifelse(ha=="home",awaygls,homegls),
      xgf=ifelse(ha=="home",homexg,awayxg),
      xga=ifelse(ha=="home",awayxg,homexg),
      #   xGF538=ifelse(ha=="home",xGHome538,xGAway538),
      #   xGA538=ifelse(ha=="home",xGAway538,xGHome538),
      #   spiF=ifelse(ha=="Home",spi1,spi2),
      #   spiA=ifelse(ha=="Home",spi2,spi1),
      #   probF=ifelse(ha=="Home",prob1,prob2),
      #   probA=ifelse(ha=="Home",prob2,prob1),
      #   proj_scoreF=ifelse(ha=="Home",proj_score1,proj_score2),
      #   proj_scoreA=ifelse(ha=="Home",proj_score2,proj_score1),
      #   importanceF=ifelse(ha=="Home",importance1,importance2),
      #   importanceA=ifelse(ha=="Home",importance2,importance1),
      #   nsxGF538=ifelse(ha=="Home",nsxGHome538,nsxGAway538),
      #   nsxGA538=ifelse(ha=="Home",nsxGAway538,nsxGHome538),
      #   adj_scoreF=ifelse(ha=="Home",adj_score1,adj_score2),
      #   adj_scoreA=ifelse(ha=="Home",adj_score2,adj_score1)
    )
  
  return(matches_long)
}

make_long_matches_wfr <- function(matches){ # transform matches to long format
  
  matches_long <-
    matches %>%
    pivot_longer(cols=c(Home_Team,Away_Team),
                 names_to="H_A",
                 values_to="Squad") %>%
    left_join(matches) %>%
    mutate(
      opposition=ifelse(H_A=="Home",Away_Team,Home_Team),
      glsf=ifelse(H_A=="Home",Home_Score,Away_Score),
      glsa=ifelse(H_A=="Home",Away_Score,Home_Score),
      xgf=ifelse(H_A=="Home",Home_xG,Away_xG),
      xga=ifelse(H_A=="Home",Away_xG,Home_xG)
    )
  
  return(matches_long)
}

filter_na <- function(data,cols){ # filter na
  data %<>%
    filter_at(cols,any_vars(!is.na(.)))
  
  return(data)
}

get_mva <- function(xG,n=6){ # windowed average xG
  get_weighted_mean <- function(xG,n){
    k <- length(xG)
    weights <- (1:n)/n
    weights <- weights[(n-k+1):n]
    
    xG_weighted <- list()
    divider <- list()
    
    for(i in 1:k){
      xG_weighted[[i]] <- xG[[i]]*weights[[i]]
      divider[[i]] <- weights[[i]]
    }
    
    weighted_mean <- reduce(xG_weighted,sum)/reduce(divider,sum)
    
    return(weighted_mean)
  }
  
  weighted_mean <-
    xG %>%
    as_tibble %>%
    mutate(xg_mva=slide_dbl(value,possibly(get_weighted_mean,otherwise=NA),.before=(n-1),n=n)) %>%
    pull(xg_mva)
  
  return(weighted_mean)
}

get_unused_subs <- function(data,Subs_Available,Subs_Used){
  Subs_Unused <- Subs_Available-Subs_Used
  if(Subs_Unused<=0){
    return(data)
  } else {
    for(n in 1:Subs_Unused){
      data <-
        data %>%
        add_row(Event_Time=-3,Game_State="Unused")
    }
    return(data)
  }
}

get_opposition <- function(team=NA,home_away=NA,home_team,away_team){
  opposition <- ifelse(home_away=="Home",away_team,home_team)
  # opposition <- ifelse(team==home_team,away_team,home_team)
  return(opposition)
}

add_logo <- function(plots,path,x,y,hjust=1,vjust=1,width=0.1,height=0.1,scale=1){
  plots_logo <- list()
  
  for(i in 1:length(plots)){
    name <- names(plots[i])
    plots_logo[[name]] <- 
      plots[[i]] %>%
      ggdraw() +
      draw_image(path,
                 x=x,
                 y=y,
                 hjust=hjust,
                 vjust=vjust,
                 width=width,
                 height=height,
                 scale=scale)
  }
  return(plots_logo)
}

save_plots <- function(plots,path,device="jpg",dpi=1500){
  for(i in 1:length(plots)){
    name <- names(plots[i])
    save_plot(plot=plots[[i]],filename=paste0(name,".",device),path=path,dpi=dpi)
  }
}

save_tables <- function(tables,path,extention="png"){
  for(i in 1:length(tables)){
    name <- names(tables[i])
    tables[[i]] %>%
      gtsave(filename=glue("{name}.{extention}"),path=glue("{path}"))
  }
}

padj <- function(stat,possession){
  stat_padj <- stat * 2/(1 + exp(-0.1*(team_possession-50)))
  return(stat_padj)
}