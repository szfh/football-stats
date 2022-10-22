font_add_google("Roboto", "roboto")
# font_families()
showtext_auto(enable=TRUE)
showtext_opts(dpi=1500)

theme <- list()
theme$base_size <- 12
theme$base_family <- "roboto"

theme$white <- function(base_size=theme$base_size,base_family=theme$base_family){
  theme_bw(base_size=base_size,base_family=base_family) +
    theme(
      axis.ticks=element_blank(),
      axis.line=element_line(size=0.6),
      axis.text.x=element_text(hjust=0.5,vjust=0.5,size=6),
      axis.text.y=element_text(hjust=0.5,vjust=0.5,size=6),
      
      legend.position="none",
      
      panel.border=element_blank(),
      panel.background=element_blank(),
      
      panel.grid.major=element_line(size=0.4),
      panel.grid.minor=element_blank(),
      
      plot.title=element_text(size=12,face="bold",hjust=0.5),
      plot.subtitle=element_text(size=10,hjust=0.5),
      plot.caption=element_text(size=6),
      
      plot.margin=unit(c(0.5,0.5,0.5,0.5),"lines"),
      
      strip.background=element_blank(),
      strip.text=element_text(colour="black",face="bold",angle=0)
    )
}

# https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/
theme$solar <- function(base_size=theme$base_size,base_family=theme$base_family){
  theme_solarized(base_size=base_size,base_family=base_family) +
    theme(
      axis.ticks=element_blank(),
      axis.line=element_line(size=0.6),
      axis.line.x=element_blank(),
      axis.line.y=element_blank(),
      axis.title=element_text(size=10,colour="black",face="bold",hjust=0.5,vjust=0.5),
      axis.text=element_text(size=8,colour="black",hjust=0.5,vjust=0.5),
      
      legend.position="none",
      
      panel.border=element_blank(),
      panel.background=element_blank(),
      
      panel.grid.major=element_line(size=0.4),
      panel.grid.minor=element_blank(),
      
      plot.title=element_text(size=12,colour="black",face="bold",hjust=0),
      plot.subtitle=element_text(size=8,colour="black",hjust=0),
      plot.caption=element_text(size=6,colour="black",hjust=0),
      
      plot.margin=unit(c(0.5,0.5,0.5,0.5),"lines"),
      
      strip.background=element_blank(),
      strip.text=element_text(colour="black",face="bold",angle=0)
    )
}

theme$solarfacet <- function(base_size=theme$base_size,base_family=theme$base_family){
  theme$solar() +
    theme(
      axis.line.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.x=element_blank(),
      axis.title.x=element_blank(),
      panel.grid.major.x=element_blank()
    )
}

theme$dark <- function(base_size=theme$base_size,base_family=theme$base_family){
  theme_solarized(base_size=theme$base_size,base_family=theme$base_family, light=FALSE) +
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
      strip.text=element_text(face="bold",angle=0)
    )
}

# palettes
palette <- list()
palette$epl <- function(team_name){
  colours <- c(
    `Other` = "lightgrey",
    `Arsenal` = "#EF0107",
    `Aston Villa` = "#670E36",
    `AFC Bournemouth` = "#DA291C",
    `Brentford` = "#E30613",
    `Brighton & Hove Albion` = "#0057B8",
    `Brighton` = "#0057B8",
    `Bournemouth` = "#DA291C",
    `Burnley` = "#6C1D45",
    `Cardiff City` = "#0070B5",
    `Chelsea` = "#034694",
    `Crystal Palace` = "#1B458F",
    `Everton` = "#003399",
    `Fulham` = "#FFFFFF",
    `Huddersfield` = "#0E63AD",
    `Leeds` = "#FFE100",
    `Leeds United` = "#FFE100",
    `Leeds Utd` = "#FFE100",
    `Leicester City` = "#003090",
    `Liverpool` = "#C8102E",
    `Manchester City` = "#6CABDD",
    `Manchester United` = "#DA291C",
    `Manchester Utd` = "#DA291C",
    `Newcastle` = "#241F20",
    `Newcastle United` = "#241F20",
    `Newcastle Utd` = "#241F20",
    `Norwich City` = "#FFEE00",
    `Nottingham Forest` = "#e53233",
    `Sheffield United` = "#EE2737",
    `Sheffield Utd` = "#EE2737",
    `Southampton` = "#D71920",
    `Stoke City` = "#E03A3E",
    `Swansea City` = "#121212",
    `Tottenham Hotspur` = "#132257",
    `Tottenham` = "#132257",
    `Watford` = "#FBEE23",
    `West Bromwich Albion` = "#000080",
    `West Brom` = "#000080",
    `WBA` = "#000080",
    `West Ham` = "#7A263A",
    `West Ham United` = "#7A263A",
    `West Ham Utd` = "#7A263A",
    `Wolverhampton Wanderers` = "#FDB913",
    `Wolves` = "#FDB913"
  )
  
  return(colours[team_name])
}

palette$cpl <- function(team_name){
  colours <- c(
    `Other` = "lightgrey",
    `Pacific` = "#8c1aff",
    `Edmonton` = "#4d79ff",
    `Cavalry` = "#ff4d4d",
    `Valour` = "#990000",
    `Forge` = "#ffa64d",
    `York9` = "#66cc00",
    `HFX Wanderers` = "#80bfff",
    `Atlético Ottawa` = "#e62e00",
    `Atletico Ottawa` = "#e62e00"
  )
  
  return(colours[team_name])
}

palette$cpl2 <- function(team_id){
  colours <- c(
    `Other` = "lightgrey",
    `15380` = "#8c1aff",
    `6925` = "#4d79ff",
    `15381` = "#ff4d4d",
    `15378` = "#990000",
    `15377` = "#ffa64d",
    `15382` = "#66cc00",
    `15376` = "#80bfff",
    `16614` = "#e62e00"
  )
  
  return(colours[team_id])
}

palette$sfc <- function(p){
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
colour$light <- few_pal("Light")(8)
colour$medium <- few_pal("Medium")(8)
colour$dark <- few_pal("Dark")(8)
colour$gdocs <- gdocs_pal()(10)
colour$sfc <- c(
  "main"="#D71920",
  "light"="#ED5C5C",
  "grey"=ggthemes_data$solarized$Base$value[[5]],
  "solar"=ggthemes_data$solarized$Base$value[[7]],
  "black"="#000000"
)

# captions
caption <- list(
  "data: statsbomb/fbref",
  "data: statsbomb/fbref\nprepared by: @saintsbynumbers"
)

# tests
# colorspace::swatchplot(colour$medium)
# colorspace::swatchplot(ggthemes_data$solarized$Base$value)
