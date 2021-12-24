# install.packages("tidyverse","ggplot2","worldfootballR") # run this line to install packages used in this script
{
  library(tidyverse) # load packages
  library(ggplot2)
  library(worldfootballR)
}

# download data
results <- worldfootballR::get_match_results("ENG","M",2022) # download data from fbref

# organise data
results_pivot <-
  results %>% # start with the downloaded data
  filter(!is.na(HomeGoals)) %>% # remove games that haven't been played yet
  select(Home,Home_xG,Away,Away_xG) %>% # remove columns that aren't needed
  pivot_longer(c("Home","Away"),names_to="Home_Away",values_to="Team") %>% # pivot from wide to long format
  mutate( # from home/away to for/against
    xG_For=ifelse(Home_Away=="Home",Home_xG,Away_xG),
    xG_Against=ifelse(Home_Away=="Home",Away_xG,Home_xG),
    .keep="unused"
  )

# create visual
xg_facet_plot <-
  results_pivot %>%
  ggplot(aes(x=xG_For,y=xG_Against,fill=(xG_For-xG_Against))) + # start of plot, assign aesthetics
  geom_point(shape=21,size=0.8,stroke=0.2) + # scatter plot
  geom_abline(intercept=0,slope=1,size=0.1) + # add line
  facet_wrap(. ~ Team) + # split to one facet per team
  theme_minimal() + # simple default theme
  coord_fixed() + # fixed aspect ratio
  theme( # minor visual changes
    legend.position="none",
    plot.title=element_text(size=8),
    plot.caption=element_text(size=4),
    strip.text=element_text(size=4),
    axis.title=element_text(size=6),
    axis.text=element_text(size=4),
    panel.border=element_rect(size=0.2,fill=NA),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank()
  ) +
  labs( # titles
    title="xG By Game, Premier League",
    x="xG For",
    y="xG Against",
    caption="Data: Statsbomb / FBRef"
  ) +
  scale_x_continuous(expand=expansion(add=c(0.1,0.2))) + # set axis limits
  scale_y_continuous(expand=expansion(add=c(0.1,0.2))) + # set axis limits
  scale_fill_gradient2(low="darkblue",mid="pink",high="red") # set point fill gradients

# save
ggsave(here::here("plots","test","xg_facet_plot.jpg"),xg_facet_plot,height=4,width=4,dpi=1500) # save to file location you choose
# save_path <- "C:/Users/Your_Name_Here/Desktop/xg_facet_plot.jpg" # type your save path here
# ggsave(filename=save_path,plot=xg_plot,height=4,width=4,dpi=1500) # save plot
