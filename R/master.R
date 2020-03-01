# libraries and themes
source(here::here("R","library.R"))
source(here("R","themes.R"))
source(here("R","utils.R"))

# knit markdown
# knitr::knit(here::here("R","README.Rmd"))

# raw
# source(here("R","raw","fbref.R"))
# source(here("R","raw","fivethirtyeight.R"))

# tidy
source(here("R","tidy","fbref.R"))
source(here("R","tidy","fivethirtyeight.R"))

# join
source(here("R","join","join.R"))

# graph
source(here("R","plot","plot_sfc.R"))
source(here("R","plot","plot_epl.R"))
