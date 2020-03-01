# latest_data <- matches %>%
#   filter(!is.na(GoalsHome)&!is.na(GoalsAway)) %>%
#   summarise(last(Date)) %>%
#   extract2(1)

# windowed average xG
get_mva <- function(xG,n=6){
  
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
