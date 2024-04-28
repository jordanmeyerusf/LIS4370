#' Add arrow annotation to a plot
#'
#' @param plot The ggplot object to which the arrow annotation will be added.
#' @param x The x-coordinate of the starting point of the arrow.
#' @param y The y-coordinate of the starting point of the arrow.
#' @param xend The x-coordinate of the ending point of the arrow.
#' @param yend The y-coordinate of the ending point of the arrow.
#' @param color The color of the arrow.
#'
#' @return A ggplot object with the added arrow annotation.
#'
#' @export
add_arrow <- function(plot, x, y, xend, yend, color) {
  plot + geom_segment(aes(x = x, y = y, xend = xend, yend = yend), color = color, arrow = arrow())
}
