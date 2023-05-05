expand_seasons <- function(season){
  season <- c(season,str_sub(season,-4))
  # season <- distinct(season)
  return(season)
}

shorten_team_names <- function(team){
  
  team <-
    team %>%
    str_replace("United","Utd")
  
  team <-
    case_when(
      team %in% "Brighton & Hove Albion" ~ "Brighton",
      team %in% "Huddersfield Town" ~ "Huddersfield",
      team %in% "Tottenham Hotspur" ~ "Tottenham",
      team %in% "West Bromwich Albion" ~ "West Brom",
      team %in% "Wolverhampton Wanderers" ~ "Wolves",
      .default=team
    )
  
  return(team)
}

make_long_data <- function(data,levels,labels){ # transform data to long format
  
  long_data <-
    data %>%
    filter_at(levels,any_vars(!is.na(.))) %>%
    pivot_longer(cols=levels,names_to="key",values_to="n") %>%
    mutate(key=factor(key,levels=levels,labels=labels)) %>%
    group_by(key)
  
  return(long_data)
}

get_mva <- function(x,n=10){ # windowed average xG
  get_weighted_mean <- function(x,n){
    k <- length(x)
    weights <- (1-exp(-1))^((n-1):0)
    weights <- weights[(n-k+1):n]
    
    x_weighted <- list()
    divider <- list()
    
    for(i in 1:k){
      x_weighted[[i]] <- x[[i]]*weights[[i]]
      divider[[i]] <- weights[[i]]
    }
    
    weighted_mean <- reduce(x_weighted,sum)/reduce(divider,sum)
    
    return(weighted_mean)
  }
  
  weighted_mean <-
    x %>%
    as_tibble %>%
    mutate(x_mva=slide_dbl(value,possibly(get_weighted_mean,otherwise=NA),.before=(n-1),n=n)) %>%
    pull(x_mva)
  
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
  stat_padj <- stat * 2/(1 + exp(-10*(possession-0.5)))
  return(stat_padj)
}