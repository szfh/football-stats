expand_seasons <- function(season){
  season <- c(season,str_sub(season,-4))
  return(season)
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
  
  xGlag <- list()
  divider <- list()
  
  for(i in 1:n){
    divider[[i]] <- (1-((i-1)/n))
    # xGlag[[i]] <- lag(xG,(i-1))
    xGlag[[i]] <- lag(xG,(i-1))*divider[[i]]
  }
  
  # mva <-
  #   xGlag %>%
  #   as.data.frame %>%
  #   rowMeans(na.rm=TRUE)
  
  mva <-
    xGlag %>%
    as.data.frame %>%
    rowSums(na.rm=TRUE) %>%
    divide_by(reduce(divider,sum))
  
  return(mva)
}

get_unused_subs <- function(data,subs_available,subs_used){
  
  subs_unused <- subs_available-subs_used
  
  if(subs_unused<=0){
    return(data)
  }
  else{
    for(n in 1:subs_unused){
      # browser()
      data <-
        data %>%
        add_row(time=-3,state2="Unused")
    }
    return(data)
  }
}

# latest_data <- matches %>%
#   filter(!is.na(GoalsHome)&!is.na(GoalsAway)) %>%
#   summarise(last(Date)) %>%
#   extract2(1)

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

save_plots <- function(plots,path){
  for(i in 1:length(plots)){
    name <- names(plots[i])
    save_plot(glue("{path}/{name}.jpg"),plots[[i]],dpi=1000)
  }
}
