#' Create animated plots using gganimate with tidy evaluation
#'
#' @param data The data frame containing the data to be plotted.
#' @param x The variable to be plotted on the x-axis.
#' @param y The variable to be plotted on the y-axis.
#' @param frame The variable used for animation frames.
#' @param title The title of the plot.
#' @param xlab The label for the x-axis.
#' @param ylab The label for the y-axis.
#' @param duration The duration of the animation in seconds.
#'
#' @return An animated ggplot object.
#'
#' @export
pretty_animate_plot <- function(data, x, y, frame, title = NULL, xlab = NULL, ylab = NULL, duration = 5) {
  # Load required packages
  library(ggplot2)
  library(gganimate)
  
  # Create initial plot
  p <- ggplot(data, aes(x = {{x}}, y = {{y}}, frame = {{frame}})) +
    geom_point() +
    labs(title = title, x = xlab, y = ylab) +
    theme_minimal()
  
  # Animate the plot
  anim <- p + transition_states(states = {{frame}}, transition_length = 2, state_length = duration) +
    enter_fade() + exit_fade()  # Add smooth transitions
  
  return(anim)
}
