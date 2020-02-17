# themes
theme_sfc <- function(base_size=12,base_family="sans"){
  theme_bw(base_size=base_size,base_family=base_family) +
    theme(
      axis.ticks=element_blank(),
      axis.line=element_line(size=0.6),
      axis.text=element_text(hjust=0.5,vjust=0.5,size=rel(1)),
      
      legend.position="none",
      
      strip.background=element_blank(),
      strip.text=element_text(colour="black",face="bold",angle=0),
      
      panel.border=element_blank(),
      panel.background=element_blank(),
      
      panel.grid.major=element_line(size=0.4),
      panel.grid.minor=element_blank(),
      
      plot.title=element_text(size=rel(1.2),face="bold",hjust=0.5),
      plot.subtitle=element_text(size=rel(1),hjust=0.5),
      plot.caption=element_text(size=rel(0.8)),
      
      plot.background=element_blank(),
      plot.margin=unit(c(1,1,0.5,0.5),"lines"),
    )
}

theme_epl <- theme_sfc

# palettes
palette_epl <- function(team_name){
  palette_epl <- c(
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
  
  # if(is.null(palette_epl))
  #   return(palette_epl)
  
  return(palette_epl[team_name])
}

palette_sfc <- function(p){
  palette_sfc <- c(
    `main` = "#D71920", # normal
    `light` = "#ED5C5C", # light
    `black` = "#000000",
    `lightgrey` = "#D3D3D3"
  )
  
  # if(is.null(palette_sfc))
  #   return(palette_sfc)
  
  return(palette_sfc[p])
}

# colour lists
col_light <- few_pal("Light")(8)
col_medium <- few_pal("Medium")(8)
col_dark <- few_pal("Dark")(8)
col_gdocs <- gdocs_pal()(10)
col_sfc <- c("#D71920","#ED5C5C","#D3D3D3")

# captions
caption <- c(
  "data: statsbomb/@fbref",
  "data: statsbomb/@fbref\nprepared: @saintsbynumbers"
)

# tests
# colorspace::swatchplot(col_medium)
# colorspace::swatchplot(col_sfc)
# colorspace::swatchplot(palette_epl())
# colorspace::swatchplot(palette_sfc())
