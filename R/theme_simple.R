options(
  ggplot2.discrete.colour = ggokabeito::palette_okabe_ito(),
  ggplot2.discrete.fill = ggokabeito::palette_okabe_ito(),
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

# Insipred by https://github.com/vankesteren/firatheme/ and https://github.com/traffordDataLab/trafford_themes
theme_simple <- function(base_family = "", base_size = 12,
                         bg_color = "white") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg_color, color = bg_color),
      panel.background = ggplot2::element_rect(fill = bg_color, color = bg_color),
      panel.border = ggplot2::element_blank(),
      
      legend.background = ggplot2::element_rect(fill = "transparent",
                                                colour = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      strip.background = ggplot2::element_rect(fill = "transparent",
                                               colour = NA),
      
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),

      plot.title = ggplot2::element_text(size = base_size + 3, colour = "black",
                                         hjust = 0,
                                         margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(size = base_size + 1, colour = "gray40",
                                            hjust = 0,
                                            margin = ggplot2::margin(b = 15)),
      plot.caption = ggplot2::element_text(size = max(base_size - 3, 9), 
                                           colour = "gray40", hjust = 1,
                                           margin = ggplot2::margin(t = 12)),
      axis.title.x = ggplot2::element_text(size = base_size, colour = "#454545", 
                                           hjust = 1, margin = ggplot2::margin(t = 6)),
      axis.title.y = ggplot2::element_text(size = base_size, colour = "#454545", 
                                           hjust = 1, vjust = 1, angle = 90,
                                           margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
      axis.text.y = ggplot2::element_text(size = base_size - 2, colour = "#454545"),
      axis.text.x = ggplot2::element_text(size = base_size - 2, colour = "#454545", 
                                          ggplot2::margin = margin(t = -5)),
      legend.title = ggplot2::element_text(size = base_size, colour = "#454545"),
      legend.text = ggplot2::element_text(size = base_size - 2, colour = "#454545"),
      strip.text = ggplot2::element_text(hjust = 0, size = base_size + 1, 
                                         colour = "#454545", 
                                         margin = ggplot2::margin(10, 10,  10, 10, "pt")),
      legend.position = "bottom"
    )
}