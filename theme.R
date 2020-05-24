waffle_theme_custom <- theme(
  plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
  plot.subtitle = element_text(hjust = 0.5, size = 10, face = "italic"),
  panel.background = element_rect(fill = "transparent", colour = NA),
  legend.text = element_text(hjust = 0.5, size = 9),
  legend.title = element_blank(),
  legend.key = element_rect(size = 5),
  legend.position = "bottom",
  legend.justification = "center",
  legend.margin = margin(0, 0, 0, 0),
  legend.spacing.x = unit(0, "pt"),
  plot.caption = element_text(hjust = 1, face = "italic"),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  axis.text = element_blank(),
  plot.margin=unit(c(1,1,1,1), "lines", )
)

simple_line_theme <- theme(
  plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
  plot.subtitle = element_text(hjust = 0.5, size = 10, face = "italic"),
  axis.text.x = element_text(angle = 90, hjust = 1, size = 8, face = "bold"),
  axis.text.y = element_text(face ="italic"),
  legend.text = element_text(hjust = 0.5, size = 9),
  legend.position = c(0.8, 0.2),
  legend.title = element_blank(),
  legend.justification = "center",
  axis.title = element_text(hjust = 0.5, size = 10, face = "bold"),
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.margin=unit(c(1,1,1,1), "lines"),
  plot.caption = element_text(hjust = 1, face = "italic"),
)

simple_bar_theme <- theme(
  plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
  plot.subtitle = element_text(hjust = 0.5, size = 10, face = "italic"),
  axis.text.x = element_text(angle = 90, hjust = 1, size = 8, face = "bold"),
  axis.text.y = element_text(face ="italic"),
  legend.text = element_text(hjust = 0.5, size = 9),
  legend.title = element_blank(),
  legend.justification = "center",
  axis.title = element_text(hjust = 0.5, size = 10, face = "bold"),
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.margin=unit(c(1,1,1,1), "lines"),
  plot.caption = element_text(hjust = 1, face = "italic"),
)