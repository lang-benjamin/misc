theme_classical <- function(base_family = "", base_size = 12,
                            borders_gray = FALSE, bg_color = "white",
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
  title_col <- "black" # if (borders_gray) "gray30" else "black"
  title_size <- base_size + 4
  subtitle_size <- axis_title_size + 2
  subtitle_col <- "black"
  subtitle_margin <- 20
  caption_size <- max(base_size - 3, 9)
  caption_col <- "gray50"
  
  ret <- ggplot2::theme_classic(base_family = base_family, base_size = base_size)
  ret <- ret + 
    theme(legend.background = element_blank(), 
          legend.key = element_blank(), 
          strip.background = element_blank(),
          panel.grid = element_blank(), 
          axis.line = element_line(linewidth = axis_size, color = axis_col),
          axis.line.x = element_line(linewidth = axis_size, color = axis_col,
                                     arrow = if (!arrows) NULL else arrow(type = "closed", length = unit(0.06, "inches"))),
          axis.line.y = element_line(linewidth = axis_size, color = axis_col,
                                     arrow = if (!arrows) NULL else arrow(type = "closed", length = unit(0.06, "inch"))),
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
    ret <- ret + theme(plot.margin = plot_margin)
  ret
}

# For generating figures (taken from Frank Harrell)
# - Quarto setup: knitr::opts_chunk$set(dev = 'raggpng', fig.ext = 'png')
# - Use ggsave with ggsave(filename, plot, device = raggpng, dpi = 600, units = "in")
raggpng <- function(..., res = 192) ragg::agg_png(..., res = res, units = 'in')

# Mixture of Hmisc::ggfreqScatter, Hmisc::movStats and https://thestatsgeek.com/2014/09/13/checking-functional-form-in-logistic-regression-using-loess/
plot_scatter <- function(data, x, y, binary_on_logit = FALSE, 
                         fcolors = viridis::viridis(10)) {
  if (length(unique(data[[y]])) == 2) { 
    # y is binary
    data[[y]] <- as.integer(data[[y]]) - 1 # make sure it's integer based
    ms <- movStats(reformulate(x, y), melt = TRUE, data = data)
    if (binary_on_logit) {
      ms_logit <- ms
      ms$Scale <- "Original"
      ms_logit$Scale = "Logit"
      ms_logit[, (y) := lapply(.SD, qlogis), .SDcols = y]
      ms <- bind_rows(ms, ms_logit)
    
      p1 <- ggfreqScatter(x = data[[x]], y = data[[y]], fcolors = fcolors) +
        geom_line(mapping = aes(x = .data[[x]], y = .data[[y]], 
                                color = NULL, alpha = NULL, label = NULL), 
                  data = dplyr::filter(ms, Statistic == "Proportion", Scale == "Original")) +
        labs(x = paste(x), y = paste(y),
             caption = "Black line: Moving proportion (original scale)")
      p2 <- ggplot(data = dplyr::filter(ms, Statistic == "Proportion", Scale == "Logit"),
                   mapping = aes(x = .data[[x]], y = .data[[y]])) +
        geom_line() +
        labs(x = paste(x), y = paste(y),
             caption = "Black line: Logit of moving proportion") 
      return(plot_grid(p1, p2))
    } else {
      p <- ggfreqScatter(x = data[[x]], y = data[[y]], fcolors = fcolors) +
        geom_line(mapping = aes(x = .data[[x]], y = .data[[y]], 
                                color = NULL, alpha = NULL, label = NULL), 
                  data = dplyr::filter(ms, Statistic == "Proportion")) +
        labs(x = paste(x), y = paste(y),
             caption = "Black line: Moving proportion")
      return(p)
    }
  } else {
    ms <- movStats(reformulate(x, y), melt = TRUE, data = data)
    p <- ggfreqScatter(x = data[[x]], y = data[[y]], fcolors = fcolors) +
      geom_line(mapping = aes(x = .data[[x]], y = .data[[y]], 
                              alpha = NULL, label = NULL), 
                color = I('blue'), data = filter(ms, Statistic == "Mean")) +
      geom_line(mapping = aes(x = .data[[x]], y = .data[[y]], 
                              color = NULL, alpha = NULL, label = NULL), 
                data = filter(ms, Statistic == "Median")) +
      labs(x = paste(x), y = paste(y),
           caption = "Black line: moving median\nBlue line: moving mean")
    return(p)
  }
}                                  

# Plotting distribution via dotsinterval from ggdist
plot_dotsinterval <- function(data, x, bg_color = "white") {
  d_med <- summarise(data, med = median(.data[[x]], na.rm = TRUE))
  p <- data %>%
    ggplot(aes(x = .data[[x]], fill = after_stat(level))) +
    # quantile interval should cover 50% (0.25 to 0.75 quantile) and 
    # 90% (0.05 to 0.95 quantile) of the data
    stat_dotsinterval(point_interval = "median_qi", .width = c(0.5, 0.9), slab_linetype = "blank") +
    geom_text(data = d_med, aes(x = med, y = -0.005, label = round(med, 2), fill = NULL), position = position_dodge(width = 0.8), size = 3, vjust = 1.5) +
    scale_color_manual(name = "% sample covered", values = scales::brewer_pal()(3)[-1], labels = c("90%", "50%", "<10%"), aesthetics = "fill") +
    theme_classical(arrows = FALSE, bg_color = bg_color) + 
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          legend.position = "bottom")
  
  # In case data has labels attached to their variables, we will use them
  if (!is.na(label(data[x]))) p + ggeasy::easy_labs() else p # TODO: switch to Hmisc::hlabs
}

plot_grouped_dotsinterval <- function(data, x, y, g) {
  #d_med <- summarise(group_by(data, .data[[x]]), med = median(.data[[y]], na.rm = TRUE))
  p <- data %>%
    ggplot(aes(x = .data[[x]], y = .data[[y]], fill = .data[[g]])) +
    # quantile interval should cover 50% (0.25to 0.75 quantile) and 
    # 90% (0.05 to 0.95 quantile) of the data
    stat_dotsinterval(point_interval = "median_qi", .width = c(0.5, 0.9), slab_linetype = "blank") +
    scale_color_manual(name = "% sample covered", values = scales::brewer_pal()(3)[-1], labels = c("90%", "50%", "<10%"), aesthetics = "slab_fill") +
    #geom_text(data = d_med, aes(.data[[x]], med, label = round(med, 2), fill = NULL), position = position_dodge(width = 0.8), size = 3, vjust = 1.5) +
    theme_classical(arrows = FALSE) +
    coord_flip() +
    theme(axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          legend.position = "bottom")
  
  # In case data has labels attached to their variables, we will use them
  if (!is.na(label(data[x]))) p + ggeasy::easy_labs() else p # TODO: switch to Hmisc::hlabs
}
