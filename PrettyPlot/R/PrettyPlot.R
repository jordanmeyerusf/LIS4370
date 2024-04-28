install.packages("ggplot2")
install.packages("gganimate")


# Function to add text annotation to a plot
add_annotation <- function(plot, x, y, label, color) {
  plot + geom_text(aes(x = x, y = y, label = label), color = color)
}

# Function to add arrow annotation to a plot
add_arrow <- function(plot, x, y, xend, yend, color) {
  plot + geom_segment(aes(x = x, y = y, xend = xend, yend = yend), color = color, arrow = arrow())
}

# Function to apply a custom theme to a plot
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




# Function to create animated plots using gganimate with tidy evaluation
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





# Example usage of annotation functions with mtcars dataset
library(ggplot2)

# Load mtcars dataset
data(mtcars)

# Create a scatter plot of mpg vs. wt
plot <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

# Add text annotation at a specific point
plot_with_annotation <- add_annotation(plot, x = 3, y = 20, label = "High MPG", color = "blue")

# Add arrow annotation from one point to another
plot_with_arrow <- add_arrow(plot_with_annotation, x = 4, y = 15, xend = 2, yend = 30, color = "red")

# Display the plot with annotations
print(plot_with_arrow)





# Example usage of custom_theme with mtcars dataset
library(ggplot2)

# Load mtcars dataset
data(mtcars)

# Create a scatter plot of mpg vs. wt with a linear regression line
plot <- ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point() +
  geom_smooth(method = "lm") +  # Add linear regression line
  labs(x = "Weight", y = "Miles per gallon", title = "Fuel Efficiency")

# Apply the custom theme with customized visual properties
plot_with_custom_theme <- custom_theme(plot, 
                                       font_size = 14, 
                                       font_color = "darkblue",
                                       background_color = "lightgray",
                                       axis_color = "black",
                                       axis_label_size = 12,
                                       axis_title_size = 16,
                                       axis_tick_size = 10,
                                       plot_title_size = 18,
                                       legend_title_size = 14,
                                       legend_text_size = 12,
                                       show_grid = TRUE,
                                       point_color = "red",
                                       point_size = 3,
                                       line_color = "blue",
                                       line_width = 1)

# Display the plot with the custom theme
print(plot_with_custom_theme)




# Example usage of pretty_animate_plot function with iris dataset
library(ggplot2)
library(gganimate)

# Create animated scatter plot of sepal length and sepal width over time
anim_plot <- pretty_animate_plot(data = iris, x = Sepal.Length, y = Sepal.Width, frame = Species,
                                 title = "Sepal Length vs. Sepal Width by Species", xlab = "Sepal Length", ylab = "Sepal Width",
                                 duration = 3)

# Display the animated plot
anim_plot





