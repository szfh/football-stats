# how to make an xg trend line in R/tidyverse/ggplot
# feel free to copy, plagarise, improve, whatever
# stuck? got a better idea? --> @saintsbynumbers

require(tidyverse)
require(magrittr)
require(rvest)
require(glue)
require(ggtext) # for markdown in plots - https://github.com/wilkelab/ggtext
require(tidytext) # for reorder_within

# 2020-21 match data from fbref using acciotables: https://github.com/npranav10/acciotables/
url_2020_21 <- "http://acciotables.herokuapp.com/?page_url=https://fbref.com/en/comps/9/10728/schedule/&content_selector_id=%23sched_ks_10728_1"
# 2019-20 data
url_2019_20 <- "http://acciotables.herokuapp.com/?page_url=https://fbref.com/en/comps/9/3232/schedule/&content_selector_id=%23sched_ks_3232_1"

# import
matches_2020_21 <-
  url_2020_21 %>%
  read_html() %>%
  html_table() %>%
  extract2(1)

matches_2019_20 <-
  url_2019_20 %>%
  read_html() %>%
  html_table() %>%
  extract2(1)

matches <- bind_rows(matches_2019_20,matches_2020_21,.id="Season")

# tidy up the data
matches_tidy <-
  matches %>%
  filter(Wk!="Wk",Wk!="") %>% # remove non-data rows
  select(-c("Attendance":"Notes")) %>% # don't need these
  separate("Score",c("HomeGls","AwayGls"),sep="[:punct:]",fill="right") %>% # separate score to two columns
  rename("HomexG"="xG...6","AwayxG"="xG...8") %>% # give useful names
  type_convert() %>% # fix data types
  filter(!is.na(HomeGls)) # only keep matches which have been played
# you should have all completed matches in a data frame now

matches_long <-
  matches_tidy %>%
  pivot_longer(cols=c(Home,Away),
               names_to="HA",
               values_to="Squad") %>%
  left_join(matches_tidy) %>% # join the old data frame to the new one
  mutate(
    Opposition=ifelse(HA=="Home",Away,Home),
    GlsF=ifelse(HA=="Home",HomeGls,AwayGls),
    GlsA=ifelse(HA=="Home",AwayGls,HomeGls),
    xGF=ifelse(HA=="Home",HomexG,AwayxG),
    xGA=ifelse(HA=="Home",AwayxG,HomexG))
# now you should have 760 rows, one for each team in each match


get_windowed_average <- function(xG,n=6){ # windowed average xG
  # get windowed averages for xg trend line
  # calculates the average xg for the previous 6 matches
  
  xGlag <- list()
  xGlag[[1]] <- xG
  
  for(i in 2:n){
    xGlag[[i]] <- lag(xG,(i-1))
  }
  
  windowed_average <- xGlag %>%
    as.data.frame %>%
    rowMeans(na.rm=TRUE)
  
  return(windowed_average)
}

# enter your team here
team <- "Liverpool"

#get matches for 1 team
matches_team <-
  matches_long %>%
  filter(Squad==!!team) %>% # filter team
  mutate(Match=glue::glue("{Opposition} {HA} {GlsF}-{GlsA}")) %>% # make X axis names
  # mutate(Match=fct_reorder(Match, Date)) %>% # order by date
  mutate(Match=reorder_within(Match, Date, Season)) %>% # get matches in the right order
  mutate(HomexG_trend=get_windowed_average(HomexG)) %>% 
  mutate(AwayxG_trend=get_windowed_average(AwayxG))

# plot xG for/against with geom_point and geom_spline
# geom_spline is similar to geom_smooth but I find handles the start/end of seasons better
# play around with the variables spar or df to get something that's not a straight line and not just connecting every point together
# spar=0.6 seems smooth enough for me
# you have to give it a group aesthetic, use group=1
matches_team %>%
  ggplot(aes(x=Match,group=Season)) +
  geom_point(aes(y=HomexG),size=1,colour="darkred",fill="darkred",alpha=0.5,shape=23) +
  geom_line(aes(y=HomexG_trend),colour="darkred",linetype="longdash",size=0.7) +
  geom_point(aes(y=AwayxG),size=1,colour="royalblue",fill="royalblue",alpha=0.5,shape=23) +
  geom_line(aes(y=AwayxG_trend),colour="royalblue",linetype="longdash",size=0.7) +
  theme_bw() +
  theme(
    plot.title=element_markdown(),
    axis.title.y=element_markdown(),
    axis.text.x=element_text(size=6,angle=60,hjust=1)
  ) +
  labs(
    title=glue("{team} <b style='color:darkred'>attack</b> / <b style='color:royalblue'>defence</b> xG trend"),
    x=element_blank(),
    y=glue("Expected goals <b style='color:darkred'>for</b> / <b style='color:royalblue'>against</b>")
  ) +
  scale_x_reordered(expand=expansion(add=c(0.5))) +
  scale_y_continuous(limits=c(0,NA),expand=expansion(add=c(0,0.1)))
