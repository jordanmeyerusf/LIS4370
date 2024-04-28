#' Apply a custom theme to a plot
#'
#' @param plot The ggplot object to which the custom theme will be applied.
#' @param font_size The font size for text elements.
#' @param font_color The color for text elements.
#' @param background_color The background color for the plot.
#' @param axis_color The color for axis lines and ticks.
#' @param axis_label_size The font size for axis labels.
#' @param axis_title_size The font size for axis titles.
#' @param axis_tick_size The font size for axis ticks.
#' @param plot_title_size The font size for the plot title.
#' @param legend_title_size The font size for legend titles.
#' @param legend_text_size The font size for legend text.
#' @param show_grid Logical value indicating whether to show grid lines.
#' @param point_color The color for points in the plot.
#' @param point_size The size for points in the plot.
#' @param line_color The color for lines in the plot.
#' @param line_width The width for lines in the plot.
#'
#' @return A ggplot object with the applied custom theme.
#'
#' @export
custom_theme <- function(plot, 
                         font_size = 12, 
                         font_color = "black",
                         background_color = "white",
                         axis_color = "black",
                         axis_label_size = font_size, 
                         axis_title_size = font_size * 1.2, 
                         axis_tick_size = font_size * 0.8, 
                         plot_title_size = font_size * 1.5, 
                         legend_title_size = font_size * 1.2, 
                         legend_text_size = font_size,
                         show_grid = TRUE,
                         point_color = "black",
                         point_size = 2,
                         line_color = "black",
                         line_width = 0.5) {
  
  plot + 
    theme_minimal() +
    theme(
      text = element_text(size = font_size, color = font_color),
      plot.background = element_rect(fill = background_color),
      axis.line = element_line(color = axis_color),
      axis.text = element_text(size = axis_tick_size, color = axis_color),
      axis.title = element_text(size = axis_title_size, color = axis_color),
      axis.title.x = element_text(size = axis_title_size, color = axis_color),
      axis.title.y = element_text(size = axis_title_size, color = axis_color),
      axis.text.x = element_text(size = axis_label_size, color = axis_color),
      axis.text.y = element_text(size = axis_label_size, color = axis_color),
      panel.grid.major = element_line(color = ifelse(show_grid, axis_color, NA)),
      panel.grid.minor = element_line(color = ifelse(show_grid, axis_color, NA)),
      legend.title = element_text(size = legend_title_size, color = font_color),
      legend.text = element_text(size = legend_text_size, color = font_color)
    ) + 
    geom_point(color = point_color, size = point_size) +
    geom_smooth(color = line_color, size = line_width)
}
