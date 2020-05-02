# themes
theme <- list()
theme[["base_size"]] <- 12
theme[["base_family"]] <- "sans"

theme[["white"]] <- function(base_size=theme[["base_size"]],base_family=theme[["base_family"]]){
  theme_bw(base_size=base_size,base_family=base_family) +
    theme(
      axis.ticks=element_blank(),
      axis.line=element_line(size=0.6),
      axis.text=element_text(hjust=0.5,vjust=0.5,size=rel(1)),
      
      legend.position="none",
      
      panel.border=element_blank(),
      panel.background=element_blank(),
      
      panel.grid.major=element_line(size=0.4),
      panel.grid.minor=element_blank(),
      
      plot.title=element_text(size=rel(1.2),face="bold",hjust=0.5),
      plot.subtitle=element_text(size=rel(1),hjust=0.5),
      plot.caption=element_text(size=rel(0.8)),
      
      plot.margin=unit(c(0.5,0.5,0.5,0.5),"lines"),
      
      strip.background=element_blank(),
      strip.text=element_text(colour="black",face="bold",angle=0),
    )
}

# https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/
theme[["solar"]] <- function(base_size=theme[["base_size"]],base_family=theme[["base_family"]]){
  theme_solarized(base_size=base_size,base_family=base_family) +
    theme(
      axis.ticks=element_blank(),
      axis.line=element_line(size=0.6),
      axis.title=element_text(size=rel(1),face="bold",hjust=0.5,vjust=0.5),
      axis.text=element_text(size=rel(1),hjust=0.5,vjust=0.5),
      
      legend.position="none",
      
      panel.border=element_blank(),
      panel.background=element_blank(),
      
      panel.grid.major=element_line(size=0.4),
      panel.grid.minor=element_blank(),
      
      plot.title=element_text(size=rel(1.2),face="bold",hjust=0.5),
      plot.subtitle=element_text(size=rel(1),hjust=0.5),
      plot.caption=element_text(size=rel(0.8),),
      
      plot.margin=unit(c(0.5,0.5,0.5,0.5),"lines"),
      
      strip.background=element_blank(),
      strip.text=element_text(face="bold",angle=0),
    )
}

theme[["dark"]] <- function(base_size=theme[["base_size"]],base_family=theme[["base_family"]]){
  theme_solarized(base_size=theme[["base_size"]],base_family=theme[["base_family"]], light=FALSE) +
    theme(
      axis.ticks=element_blank(),
      axis.line=element_line(size=0.6),
      axis.text=element_text(hjust=0.5,vjust=0.5,size=rel(1)),
      
      legend.position="none",
      
      panel.border=element_blank(),
      panel.background=element_blank(),
      
      panel.grid.major=element_line(size=0.4),
      panel.grid.minor=element_blank(),
      
      plot.title=element_text(size=rel(1.2),face="bold",hjust=0.5),
      plot.subtitle=element_text(size=rel(1),hjust=0.5),
      plot.caption=element_text(size=rel(0.8)),
      
      plot.margin=unit(c(0.5,0.5,0.5,0.5),"lines"),
      
      strip.background=element_blank(),
      strip.text=element_text(face="bold",angle=0),
    )
}

# palettes
palette <- list()
palette[["epl"]] <- function(team_name){
  colours <- c(
    `Other` = "lightgrey",
    `Arsenal` = "#EF0107",
    `Aston Villa` = "#670E36",
    `Brighton & Hove Albion` = "#0057B8",
    `Brighton` = "#0057B8",
    `Bournemouth` = "#DA291C",
    `Burnley` = "#6C1D45",
    `Chelsea` = "#034694",
    `Crystal Palace` = "#1B458F",
    `Everton` = "#003399",
    `Leicester City` = "#003090",
    `Liverpool` = "#C8102E",
    `Manchester City` = "#6CABDD",
    `Manchester United` = "#DA291C",
    `Manchester Utd` = "#DA291C",
    `Newcastle United` = "#241F20",
    `Newcastle Utd` = "#241F20",
    `Norwich City` = "#FFEE00",
    `Sheffield United` = "#EE2737",
    `Sheffield Utd` = "#EE2737",
    `Southampton` = "#D71920",
    `Tottenham Hotspur` = "#132257",
    `Tottenham` = "#132257",
    `Watford` = "#FBEE23",
    `West Ham United` = "#7A263A",
    `West Ham` = "#7A263A",
    `Wolverhampton Wanderers` = "#FDB913",
    `Wolves` = "#FDB913"
  )
  
  # if(is.null(colours))
  #   return(colours)
  
  return(colours[team_name])
}

palette[["sfc"]] <- function(p){
  colours <- c(
    "main" = "#D71920", # normal
    "light" = "#ED5C5C", # light
    "lightgrey" = "#D3D3D3",
    "black" = "#000000"
  )
  
  # if(is.null(colours))
  #   return(colours)
  
  return(colours[p])
}

# colours
colour <- list()
colour[["light"]] <- few_pal("Light")(8)
colour[["medium"]] <- few_pal("Medium")(8)
colour[["dark"]] <- few_pal("Dark")(8)
colour[["gdocs"]] <- gdocs_pal()(10)
colour[["sfc"]] <- c(
  "main"="#D71920",
  "light"="#ED5C5C",
  "grey"=ggthemes_data[["solarized"]][["Base"]][["value"]][[5]],
  "solar"=ggthemes_data[["solarized"]][["Base"]][["value"]][[7]],
  "black"="#000000"
)

# captions
caption <- list(
  "data: statsbomb/fbref",
  "data: statsbomb/fbref\nprepared by: @saintsbynumbers"
)

# tests
# colorspace::swatchplot(colour[["medium"]])
# colorspace::swatchplot(ggthemes_data[["solarized"]][["Base"]][["value"]])
