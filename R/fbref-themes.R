theme_sfc <- function(base_size=12,base_family=""){
  theme_bw(base_size=base_size,base_family=base_family) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(size = base_size),
      plot.background=element_blank(),
      plot.margin=unit(c(0.5,0.5,0.5,0.5),"lines"),
      plot.title=element_text(size=16),
      plot.subtitle=element_text(size=12),
      plot.caption=element_text(size=8),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_line(size=0.4),
      panel.grid.minor=element_line(size=0.1,
                                    linetype="dashed"),
      axis.ticks=element_blank(),
      axis.line=element_blank()
    )
}

theme_epl <- function(base_size=12,base_family=""){
  theme_clean(base_size=base_size,base_family=base_family) +
    theme(
      # strip.background = element_blank(),
      # strip.text = element_text(size = base_size),
      # plot.background=element_blank(),
      # plot.margin=unit(c(0.5,0.5,0.5,0.5),"lines"),
      # plot.title=element_text(size=16),
      # plot.subtitle=element_text(size=12),
      # plot.caption=element_text(size=8),
      # legend.position="none",
      # panel.background=element_blank(),
      # panel.border=element_blank(),
      # panel.grid.major=element_line(size=0.4),
      # panel.grid.minor=element_line(size=0.1,
      #                               linetype="dashed"),
      # axis.ticks=element_blank(),
      # axis.line=element_blank()
    )
}

# https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
# https://encycolorpedia.com/teams/football/epl
palette_epl <- function(team_name){
  palette_epl <- c(
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
  
  if(is.null(palette_epl))
    return(palette_epl)
  
  return(palette_epl[team_name])
}
# pie(rep(1, length(palette_epl)), col = palette_epl)

col_light <- few_pal("Light")(8)
col_medium <- few_pal("Medium")(8)
col_dark <- few_pal("Dark")(8)
col_gdocs <- gdocs_pal()(10)

sfc <- "#D71920"
sfc_light <- "#ED5C5C"
