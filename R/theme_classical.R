theme_classical <- function(base_family = "", base_size = 12,
                            borders_gray = TRUE, bg_color = "white",
                            arrows = TRUE, plot_margin = NULL) {
  # default plot margin ggplot2::theme_classic()$plot.margin
  axis_col <- if (borders_gray) "gray70" else "black" # axis line and tick mark color
  axis_text_col <- if (borders_gray) "gray40" else "black" # tick mark labels and labels for axes
  axis_size <- 0.2
  axis_text_size <- base_size - 2 # tick mark label
  axis_title_size <- base_size # y or x axis title/label
  axis_title_family <- base_family
  strip_text_size <- base_size + 1
  strip_text_col <- if (borders_gray) "gray35" else "black" 
  title_col <- "black"
  title_size <- base_size + 2
  subtitle_size <- axis_title_size + 1
  subtitle_col <- "black"
  subtitle_margin <- 20
  caption_size <- max(base_size - 3, 9)
  caption_col <- "gray50"
  
  ret <- ggplot2::theme_classic(base_family = base_family, base_size = base_size)
  ret <- ret + 
    ggplot2::theme(legend.background = element_blank(), 
          legend.key = element_blank(), 
          strip.background = element_blank(),
          panel.grid = element_blank(), 
          axis.line = element_line(linewidth = axis_size, color = axis_col),
          axis.line.x = element_line(linewidth = axis_size, color = axis_col,
                                     arrow = if (!arrows) NULL else arrow(type = "closed", length = unit(0.06, "inches"))),
          axis.line.y = element_line(linewidth = axis_size, color = axis_col,
                                     arrow = if (!arrows) NULL else arrow(type = "closed", length = unit(0.06, "inches"))),
          axis.ticks = element_line(linewidth = axis_size, color = axis_col),
          axis.ticks.x = element_line(linewidth = axis_size, color = axis_col),
          axis.ticks.y = element_line(linewidth = axis_size, color = axis_col),
          axis.ticks.length = grid::unit(4, "pt"),
          axis.text.x = element_text(size = axis_text_size, color = axis_text_col),
          axis.text.y = element_text(size = axis_text_size, color = axis_text_col),
          axis.title = element_text(size = axis_title_size,
                                    family = axis_title_family,
                                    color = axis_text_col), 
          axis.title.x = element_text(size = axis_title_size,
                                      family = axis_title_family,
                                      color = axis_text_col,
                                      hjust = 1, vjust = -1),
          axis.title.y = element_text(size = axis_title_size,
                                      family = axis_title_family,
                                      color = axis_text_col,
                                      hjust = 1, vjust = 2,
                                      margin = margin(t = 0, r = 6, b = 0, l = 0)),
          strip.text = element_text(hjust = 0, size = strip_text_size, 
                                    family = base_family, color = strip_text_col),
          plot.title = element_text(size = title_size, family = base_family,
                                    color = title_col, hjust = 0,
                                    margin = margin(b = 8)),
          plot.subtitle = element_text(size = subtitle_size, family = base_family,
                                       color = subtitle_col, hjust = 0,
                                       margin = margin(b = subtitle_margin)),
          plot.caption = element_text(size = caption_size, family = base_family,
                                      color = caption_col, hjust = 1,
                                      margin = margin(t = 12)),
          legend.title = element_text(color = axis_text_col),
          legend.text = element_text(color = axis_text_col),
          plot.background = element_rect(fill = bg_color, color = bg_color),
          panel.background = element_rect(fill = bg_color, color = bg_color),
          panel.border = element_blank()
          )
  
  if (!is.null(plot_margin)) 
    ret <- ret + ggplot2::theme(plot.margin = plot_margin)

  ret
}
