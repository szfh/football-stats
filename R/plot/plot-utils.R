make_long_data <- function(data,levels,labels){ # transform data to long format
  data %<>%
    filter_at(levels,any_vars(!is.na(.))) %>%
    pivot_longer(cols=levels,names_to="key",values_to="n") %>%
    mutate(key=factor(key,levels=levels,labels=labels)) %>%
    group_by(key)
  
  return(data)
}

make_long_matches <- function(matches){ # transform matches to long format
  
  matches %<>%
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
  
  return(matches)
}

filter_season <- function(data,season="2019-20"){ # filter correct season and teams
  data %<>%
    filter(season %in% !!season)
  
  return(data)
}

# filter correct season and teams
filter_season_team <- function(data,season="2019-20",squad="Southampton"){
  data %<>%
    filter(season %in% !!season) %>%
    filter(squad %in% !!squad)
  
  return(data)
}

filter_na <- function(data,cols){ # filter na
  data %<>%
    filter_at(cols,any_vars(!is.na(.)))
  
  return(data)
}

get_mva <- function(xG,n=6){ # windowed average xG
  
  xGlag <- list()
  xGlag[[1]] <- xG
  
  for(i in 2:n){
    xGlag[[i]] <- lag(xG,(i-1))
  }
  
  mva <- xGlag %>%
    as.data.frame %>%
    rowMeans(na.rm=TRUE)
  
  return(mva)
}

# latest_data <- matches %>%
#   filter(!is.na(GoalsHome)&!is.na(GoalsAway)) %>%
#   summarise(last(Date)) %>%
#   extract2(1)

add_watermark <- function(plots,path,x,y,hjust=1,vjust=1,width=0.1,height=0.1,scale=1){
  
  plots_wm <- list()
  
  for(i in 1:length(plots)){
    name <- names(plots[i])
    plots_wm[[name]] <- 
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
  return(plots_wm)
}

save_plots <- function(plots,path){
  for(i in 1:length(plots)){
    name <- names(plots[i])
    save_plot(glue("{path}/{name}.jpg"),plots[[i]])
  }
}
