# Apply the shared labels and styling used by every plot in the app.
style_plot <- function(plot, title, x_label, y_label, legend_position = "none") {
  plot +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      legend.position = legend_position,
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank()
    )
}
