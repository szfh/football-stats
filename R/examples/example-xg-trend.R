# xg trend in ggplot

require(tidyverse)
require(magrittr)
require(rvest)
require(glue)
require(ggformula) # for geom_spline
require(ggtext) # for markdown in plots - https://github.com/wilkelab/ggtext

# 2019-20 match data from fbref using acciotables: https://github.com/npranav10/acciotables/
url <- "http://acciotables.herokuapp.com/?page_url=https://fbref.com/en/comps/9/3232/schedule/&content_selector_id=%23sched_ks_3232_1"

# import
matches_raw <-
  url %>%
  read_html() %>%
  html_table() %>%
  extract2(1)

matches <- matches_raw

# make unique column names
names(matches) <-
  names(matches) %>%
  make.unique(sep="_")

# tidy
matches <-
  matches %>%
  filter(Wk!="Wk",Wk!="") %>% # remove non-data rows
  select(-c("Attendance":"Notes")) %>% # don't need these
  separate("Score",c("HomeGls","AwayGls"),sep="[:punct:]",fill="right") %>% # separate score to two columns
  rename("HomexG"="xG","AwayxG"="xG_1") %>%
  type_convert() # fix data types
# you should have a 380 matches in a data frame now

make_long_matches <- function(matches){
  # transform matches to tidyverse long format
  # two rows per match - home and away
  # this is a super useful function which I use a lot to get match data for a single team
  
  long_matches <-
    matches %>%
    pivot_longer(cols=c(Home,Away),
                 names_to="HA",
                 values_to="Squad") %>%
    left_join(matches) %>% # join the old data frame to the new one
    mutate(
      Opposition=ifelse(HA=="Home",Away,Home),
      GlsF=ifelse(HA=="Home",HomeGls,AwayGls),
      GlsA=ifelse(HA=="Home",AwayGls,HomeGls),
      xGF=ifelse(HA=="Home",HomexG,AwayxG),
      xGA=ifelse(HA=="Home",AwayxG,HomexG)
    )
  
  return(long_matches)
}

matches_long <- make_long_matches(matches)
# now you should have 760 rows, one for each team in each match

#get matches for 1 team
aston_villa <-
  matches_long %>%
  filter(Squad=="Aston Villa") %>% # filter team
  filter(!is.na(HomeGls)) %>% # only matches which have been played
  mutate(Match=glue::glue("{Opposition} {HA} {GlsF}-{GlsA}")) %>% # make X axis names
  mutate(Match=fct_reorder(Match, Date)) # order by date

# plot xG for/against with geom_point and geom_spline
# geom_spline is similar to geom_smooth but I find handles the start/end of seasons better
# play around with the variables spar or df to get something that's not a straight line and not just connecting every point together
# spar=0.6 seems smooth enough for me
# you have to give it a group aesthetic for some reason, the workaround is use group=1
aston_villa %>%
  ggplot(aes(x=Match,group=1)) +
  geom_point(aes(y=xGF),size=2,colour="black",fill="darkred",shape=21) +
  geom_spline(aes(y=xGF),spar=0.6,colour="darkred") +
  geom_point(aes(y=xGA),size=2,colour="black",fill="royalblue",shape=21) +
  geom_spline(aes(y=xGA),spar=0.6,colour="royalblue") +
  theme_bw() +
  theme(
    plot.title=element_markdown(),
    axis.title.y=element_markdown(),
    axis.text.x=element_text(size=6,angle=60,hjust=1)
  ) +
  labs(
    title=glue("Aston Villa <b style='color:darkred'>attack</b> / <b style='color:royalblue'>defence</b> xG trend"),
    x=element_blank(),
    y=glue("Expected goals <b style='color:darkred'>for</b> / <b style='color:royalblue'>against</b>")
  ) +
  scale_x_discrete(expand=expansion(add=c(0.1))) +
  scale_y_continuous(limits=c(0,NA),expand=expansion(add=c(0,0.1)))
