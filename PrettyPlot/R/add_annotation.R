#' Add text annotation to a plot
#'
#' @param plot The ggplot object to which the annotation will be added.
#' @param x The x-coordinate of the annotation.
#' @param y The y-coordinate of the annotation.
#' @param label The text label for the annotation.
#' @param color The color of the text label.
#'
#' @return A ggplot object with the added annotation.
#'
#' @export
add_annotation <- function(plot, x, y, label, color) {
  plot + geom_text(aes(x = x, y = y, label = label), color = color)
}
