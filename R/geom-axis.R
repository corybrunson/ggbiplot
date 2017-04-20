GeomAxis <- ggproto("GeomAxis", Geom, 

  default_stat = function(.) StatIdentity,
  required_aes = c("x", "y"),
  default_aes = aes(
    xbegin = 0, 
    ybegin = 0, 
    colour = "#832424FF", 
    label = NULL, 
    size = 0.5, 
    linetype = 1, 
    alpha = NA,
    textsize = 3,
    family = "",
    fontface = 1,
    lineheight = 1.2
  ),
  guide_geom = function(.) "segment",

  draw_panel = function(data, scales, coordinates, arrow, ...) {

    if (empty(data)) return(zeroGrob())

    if (!coordinates$is_linear()) {
      warning("geom_axis does not work properly with non-linear coordinates.")
    }

    vec <- transform(data, 
      textsize = NULL, family = NULL, fontface = NULL, lineheight = NULL
    )

    gList(
      if (!is.null(data$label)) {
        text <- transform(data, 
          size = textsize,
          angle = (180/pi) * atan(y / x),
          hjust = 0.5 * (1 - 1.25 * sign(x)),
          vjust = 0.5
        )
        GeomText$draw_panel(text, scales, coordinates)
      },
      ggbiplot:::GeomVector$draw_panel(vec, scales, coordinates, arrow = arrow)
    )
  }
)

#' Axis arrows with optional text labels.
#'
#' @param arrow specification for arrow heads, as created by arrow()
#' @export
geom_axis <- function (mapping = NULL, data = NULL, stat = "identity",
  position = "identity", arrow = grid::arrow(length = unit(1/3, "picas")), 
  ...) {
  layer(mapping = mapping, data = data, stat = stat, geom = GeomAxis,
    position = position, params = list(arrow = arrow, ...))
}
