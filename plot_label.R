annotate_text2 <- function(label, x, y, facets=NULL, hjust=0, vjust=0, color='black', alpha=NA,
                           family=thm$text$family, size=thm$text$size, fontface=1, lineheight=1.0,
                           box_just=ifelse(c(x,y)<0.5,0,1), margin=unit(size/2, 'pt'), thm=theme_get()) {
  x <- scales::squish_infinite(x)
  y <- scales::squish_infinite(y)
  data <- if (is.null(facets)) data.frame(x=NA) else data.frame(x=NA, facets)
  
  tg <- grid::textGrob(
    label, x=0, y=0, hjust=hjust, vjust=vjust,
    gp=grid::gpar(col=alpha(color, alpha), fontsize=size, fontfamily=family, fontface=fontface, lineheight=lineheight)
  )
  ts <- grid::unit.c(grid::grobWidth(tg), grid::grobHeight(tg))
  vp <- grid::viewport(x=x, y=y, width=ts[1], height=ts[2], just=box_just)
  tg <- grid::editGrob(tg, x=ts[1]*hjust, y=ts[2]*vjust, vp=vp)
  inner <- grid::grobTree(tg, vp=grid::viewport(width=unit(1, 'npc')-margin*2, height=unit(1, 'npc')-margin*2))
  
  layer(
    data = NULL,
    stat = StatIdentity,
    position = PositionIdentity,
    geom = GeomCustomAnn,
    inherit.aes = TRUE,
    params = list(
      grob=grid::grobTree(inner), 
      xmin=-Inf, 
      xmax=Inf, 
      ymin=-Inf, 
      ymax=Inf
    )
  )
}
