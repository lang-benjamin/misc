# Consider using this globally
#options(
#  ggplot2.discrete.colour = ggokabeito::palette_okabe_ito(),
#  ggplot2.discrete.fill = ggokabeito::palette_okabe_ito(),
#  ggplot2.continuous.colour = "viridis",
#  ggplot2.continuous.fill = "viridis",
#)

# Basis of this theme was https://github.com/vankesteren/firatheme/
theme_simple <- function(base_family = "", base_size = 12,
                         bg_color = "white") {
  ggplot2::`%+replace%`(
    ggplot2::theme_grey(base_size = base_size, base_family = base_family),
    ggplot2::theme(
      # add padding to the plot
      plot.margin = grid::unit(rep(0.5, 4), "cm"),
      
      # remove the plot background and border
      plot.background = ggplot2::element_rect(fill = bg_color, color = bg_color),
      panel.background = ggplot2::element_rect(fill = bg_color, color = bg_color),
      panel.border = ggplot2::element_blank(),
      
      # make the legend and strip background transparent
      legend.background = ggplot2::element_rect(fill = "transparent",
                                                colour = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      strip.background = ggplot2::element_rect(fill = "transparent",
                                               colour = NA),
      
      # add light, dotted major grid lines only
      panel.grid.major = ggplot2::element_line(linetype = "dotted",
                                               colour = "#454545",
                                               linewidth = 0.2),
      panel.grid.minor = ggplot2::element_blank(),
      
      # remove the axis tick marks and hide axis lines
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "#454545", linewidth = 0.2),
      
      # modify the bottom margins of the title and subtitle
      plot.title = ggplot2::element_text(size = base_size + 3, colour = "black",
                                         hjust = 0,
                                         margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(size = base_size + 1, colour = "black",
                                            hjust = 0,
                                            margin = ggplot2::margin(b = 15)),
      
      # add padding to the caption
      plot.caption = ggplot2::element_text(size = max(base_size - 3, 9), 
                                           colour = "#454545", hjust = 1,
                                           margin = ggplot2::margin(t = 12)),
      
      # Adjust text size and axis title position
      axis.title = ggplot2::element_text(size = base_size, colour = "#454545"),
      axis.title.x = ggplot2::element_text(size = base_size, colour = "#454545", 
                                           hjust = 1, vjust = -1),
      axis.title.y = ggplot2::element_text(size = base_size, colour = "#454545", 
                                           hjust = 1, vjust = 2,
                                           margin = margin(t = 0, r = 6, b = 0, l = 0)),
      axis.text = ggplot2::element_text(size = base_size - 2, colour = "#212121"),
      legend.title = ggplot2::element_text(size = base_size, colour = "#454545"),
      legend.text = ggplot2::element_text(size = base_size - 2, colour = "#454545"),
      strip.text = ggplot2::element_text(hjust = 0, size = base_size + 1, 
                                         colour = "#454545", 
                                         margin = ggplot2::margin(10, 10,  10, 10,  "pt"))
    )
  )
}

theme_simple_facet <- function(base_family = "", base_size = 12,
                               bg_color = "white") {
  ggplot2::`%+replace%`(
    ggplot2::theme_grey(base_size = base_size, base_family = base_family),
    ggplot2::theme(
      # add padding to the plot
      plot.margin = grid::unit(rep(0.5, 4), "cm"),
      
      # remove the plot background and border
      plot.background = ggplot2::element_rect(fill = bg_color, color = bg_color),
      panel.background = ggplot2::element_rect(fill = bg_color, color = bg_color),
      panel.border = ggplot2::element_rect(color = "#454545", fill = NA),
      
      # make the legend and strip background transparent
      legend.background = ggplot2::element_rect(fill = "transparent",
                                                colour = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      strip.background = ggplot2::element_rect(fill = "transparent",
                                               colour = NA),
      
      # add light, dotted major grid lines only
      panel.grid.major = ggplot2::element_line(linetype = "dotted",
                                               colour = "#454545",
                                               linewidth = 0.2),
      panel.grid.minor = ggplot2::element_blank(),
      
      # remove the axis tick marks and hide axis lines
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "#454545", linewidth = 0.2),
      
      # modify the bottom margins of the title and subtitle
      plot.title = ggplot2::element_text(size = base_size + 3, colour = "black",
                                         hjust = 0,
                                         margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(size = base_size + 1, colour = "black",
                                            hjust = 0,
                                            margin = ggplot2::margin(b = 15)),
      
      # add padding to the caption
      plot.caption = ggplot2::element_text(size = max(base_size - 3, 9), 
                                           colour = "#454545", hjust = 1,
                                           margin = ggplot2::margin(t = 12)),
      
      # Adjust text size and axis title position
      axis.title = ggplot2::element_text(size = base_size, colour = "#454545"),
      axis.title.x = ggplot2::element_text(size = base_size, colour = "#454545", 
                                           hjust = 1, vjust = -1),
      axis.title.y = ggplot2::element_text(size = base_size, colour = "#454545", 
                                           hjust = 1, vjust = 2,
                                           margin = margin(t = 0, r = 6, b = 0, l = 0)),
      axis.text = ggplot2::element_text(size = base_size - 2, colour = "#212121"),
      legend.title = ggplot2::element_text(size = base_size, colour = "#454545"),
      legend.text = ggplot2::element_text(size = base_size - 2, colour = "#454545"),
      strip.text = ggplot2::element_text(hjust = 0, size = base_size + 1, 
                                         colour = "#454545", 
                                         margin = ggplot2::margin(10, 10,  10, 10,  "pt"))
    )
  )
}