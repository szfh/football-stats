themesfc() <- function(base_size=10,base_family=""){
  theme_bw(base_size=base_size,base_family=base_family) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(size = 12),
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