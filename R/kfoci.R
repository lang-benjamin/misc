library(KPC)
if (!requireNamespace("knockofftools", quietly = TRUE)) 
  devtools::install_github("Novartis/knockofftools")
if (!requireNamespace("energy", quietly = TRUE)) 
  install.packages("energy")

devtools::source_url("https://github.com/lang-benjamin/misc/blob/main/R/prep_data.R?raw=true")

has_heavy_ties <- function(variable, threshold = 0.5) {
  # Count frequencies of each unique value
  freq_table <- table(variable)

  # Calculate the proportion of tied observations
  tied_observations <- sum(freq_table[freq_table > 1])
  proportion_tied <- tied_observations / length(variable)

  # Check if a single value dominates the dataset
  max_tie_proportion <- max(freq_table) / length(variable)

  # Return TRUE if either the proportion of tied observations
  # or the max tie proportion exceeds the threshold
  return(proportion_tied > threshold || max_tie_proportion > threshold)
}

#  Part of the following code was inspired by qeML::qeFOCI by Norm Matloff

#' Apply the KFOCI variable selection algorithm
#' 
#' @param d A data frame containing predictors X and response y; y can be multivariate.
#' @param y_name The variable name in d that represents y; can be of length > 1.
#' @param y_yes_level If y is a factor with two levels, y_yes_level is the y value that represents 'Yes', to be coded 1 rather than 0
#' @param scale Scaling option for variables in X. 
#'    "mean_2sd" scales all continuous and integer-valued variables that are not binary by subtracting the mean and dividing by 2 times the standard deviation. 
#'    "mean_sd_all" is the counterpart but will scale all variables, including binary/dummy variables.
#'    "none" will not scale any variable.
#' @param ordered_coding How should ordinal factor variables be coded? "integer" converts the ordinal variable into an integer-valued variable (the default). For a categorical variable with k levels, "dummy" creates k-1 binary variables, "one-hot" creates k binary variables. "none" leaves the factors as they are.
#' @param unordered_coding How should non-ordinal factor variables be coded? For a categorical variable with k levels, "dummy" creates k-1 binary variables, "one-hot" creates k binary variables
#' @param Knn Same as in KPC::KFOCI but defined as function of the sample size n. Constant values are allowed.
#' @param numCores Same as numCores from KPC::KFOCI.
#' @param R Positive integer. Number of times KFOCI is called repeatedly, see details.
#'
#' @details If R > 1, KFOCI is called R times to investigate the effect of random breaking of ties for the kNN graphs. 
#'    
#' @return A list. If R = 1, then selected_indices is the return value from KFOCI and selected_names are the corresponding variable names in d.
#'    If R > 1, 'selected_indices' is the stable selection of variables according to the algorithm in Section 2.3 in Kormaksson et al. (https://doi.org/10.1002/sim.8955) and 'selected_names' are the corresponding variable names in d.
#'    The list element 'selected' contains a matrix indicating whether each of the variables was selected (1) or not (0) in each of the R repetitions, and 'ranks' contains the rank of each variable as obtained by KFOCI for each of the R runs.
#'    The element 'new_data' contains the data set after factor variables have been transformed but without scaling, restricted to the selected variables.
apply_KFOCI <- function(d, y_name, y_yes_level = NULL, 
                        scale = c("mean_2sd", "mean_sd_all", "none"),
                        ordered_coding = c("integer", "one-hot", "dummy", "none"),
                        unordered_coding = c("one-hot", "dummy"),
                        Knn = max(min(ceiling(nrow(d)/20), 20), 2),
                        numCores = parallel::detectCores(), R = 1) {
  if (!is.data.frame(d))
    stop("d should be a data frame.")
  if (is.character(y_name) && !all(y_name %in% names(d)))
    stop("y_name should be a string or a vector of strings that match the response variable(s) in d.")
  if (floor(Knn) != Knn || Knn <= 0)
    stop("Knn should be a positive integer.")
  if (floor(R) != R || R <= 0)
    stop("R should be a positive integer.")
  
  ordered_coding <- match.arg(ordered_coding)
  unordered_coding <- match.arg(unordered_coding)
  scale <- match.arg(scale)
  idx_y <- if (is.character(y_name)) match(y_name, names(d)) else y_name
  XY <- prep_data(d = d, y_name = y_name, y_yes_level = y_yes_level, 
                  remove_na = TRUE, ordered_coding = ordered_coding, 
                  unordered_coding = unordered_coding, scale = scale)
  X <- XY$X
  X_unscaled <- if (scale == "none") X else XY$X_unscaled
  Y <- XY$Y
  
  # Default bandwidth from KPC::KFOCI
  bw <- stats::median(stats::dist(Y))
  # If median is zero or if Y is binary, then use mean instead of median
  if (bw == 0 || has_heavy_ties(stats::dist(Y)))
    bw <- base::mean(stats::dist(Y))
  k <- kernlab::rbfdot(1 / (2 * bw^2))
  
  # Introduce relevance of first selected variable:
  # Check if there is an unconditional dependency to Y
  # (see KPC::KFOCI)
  p <- ncol(X)
  Q <- rep(0, p) 
  # select the first variable
  estimateQFixedY <- function(id) {
    return(KPC::TnKnn(Y, X[, id], k, Knn))
  }
  if (R == 1) {
    # Selection vector that identifies if a variable was selected or not
    S <- vector("integer", ncol(X))
    # Vector of ranks that stores rank of each variable
    # (initialize with dummy value ncol(X) + 1 as rank, corresponding to 'not selected')
    Rk <- rep.int(ncol(X) + 1, ncol(X))
    names(S) <- names(Rk) <- colnames(X)
    
    # Check first selected variable
    seq_Q <- parallel::mclapply(seq(1, p), estimateQFixedY, mc.cores = numCores)
    seq_Q <- unlist(seq_Q)
    Q[1] <- max(seq_Q)
    index_max <- min(which(seq_Q == Q[1]))
    Q1_pval <- energy::indep.test(x = X[, index_max], y = Y, R = 500)$p.value
    if (Q[1] <= 0 || Q1_pval > 0.1)
      return(list(selected_indices = integer(0),
                  selected_names = NULL,
                  selected = S,
                  ranks = Rk,
                  new_data = data.frame()))
    
    selected <- KFOCI(Y = Y, X = X, k = k, Knn = Knn, numCores = numCores)
    S[selected] <- 1
    for (i in seq_along(selected)) {
      Rk[which(colnames(X) == colnames(X)[selected[i]])] <- i
    }
    new_data <- as.data.frame(cbind(X_unscaled[, selected], Y))
    colnames(new_data)[1:(ncol(new_data) - length(idx_y))] <- colnames(X_unscaled)[selected]
    colnames(new_data)[(ncol(new_data) - length(idx_y) + 1):ncol(new_data)] <- colnames(d)[idx_y]
    return(list(selected_indices = selected, 
                selected_names = colnames(X)[selected],
                selected = S,
                ranks = Rk,
                new_data = new_data))
  } else {
    S <- matrix(0L, nrow = ncol(X), ncol = R)
    Rk <- matrix(ncol(X) + 1, nrow = ncol(X), ncol = R)
    rownames(S) <- rownames(Rk) <- colnames(X)
    colnames(S) <- colnames(Rk) <- paste0("rep_", seq_len(R))
    for (j in 1:R) {
      # Check first selected variable
      seq_Q <- parallel::mclapply(seq(1, p), estimateQFixedY, mc.cores = numCores)
      seq_Q <- unlist(seq_Q)
      Q[1] <- max(seq_Q)
      index_max <- min(which(seq_Q == Q[1]))
      Q1_pval <- energy::indep.test(x = X[, index_max], y = Y, R = 500)$p.value
      if (Q[1] <= 0 || Q1_pval > 0.1)
        return(list(selected_indices = integer(0),
                    selected_names = NULL,
                    selected = S,
                    ranks = Rk,
                    new_data = data.frame()))
      selected_vars <- KFOCI(Y = Y, X = X, 
                             k = k, Knn = Knn, numCores = numCores)
      if (!(length(selected_vars) == 1 && selected_vars == 0L)) {
        S[selected_vars, j] <- 1
        for (i in seq_along(selected_vars)) {
          Rk[which(colnames(X) == colnames(X)[selected_vars[i]]), j] <- i
        }
      }
    }
    # Check if all calls to KFOCI resulted in the same selected variables 
    # and the same ranking order (i.e. all columns of Rk are the same).
    # If so, return indices and names of the selected variables with decreasing
    # order of relevance. Otherwise return the stable selection.
    if (all(duplicated(t(Rk))[-1])) {
      sorted_idx <- order(Rk[, 1])
      selected <- sorted_idx[Rk[sorted_idx, 1] < nrow(Rk) + 1]
      selected_names <- names(Rk[selected, 1])
    } else {
      selected <- knockofftools::multi_select(S)
      selected_names <- colnames(X)[selected]
    }
    if (length(selected) == 0 && typeof(selected) == "integer") {
      new_data <- data.frame()
    } else {
      new_data <- as.data.frame(cbind(X_unscaled[, selected], Y))
      colnames(new_data)[1:(ncol(new_data) - length(idx_y))] <- colnames(X_unscaled)[selected]
      colnames(new_data)[(ncol(new_data) - length(idx_y) + 1):ncol(new_data)] <- colnames(d)[idx_y]
    }
    return(list(selected_indices = selected, 
                selected_names = selected_names, 
                selected = S, 
                ranks = Rk,
                new_data = new_data))
  }
}

#' Plot most frequent tuples of ranks from selected variables
#' 
#' @param l Result from apply_KFOCI
#' @param k Number of most frequent tuples being plotted
#' @param plot_freq Only plot variables that have an individual relative selection frequency of at least min_freq
#' @param plot_vars Plot variables in plot_vars in any case
plot_rank_tuples <- function(l, k = 5, plot_freq = 0, plot_vars = NULL) {
  Rk <- t(l$ranks)
  if (all(duplicated(Rk)[-1]))
    stop("There is no variation in the tuples of selected variables across the multiple KFOCI calls.")
  library(cdparcoord)
  idx_freq <- which(base::rowMeans(l$selected) >= plot_freq)
  idx_vars <- if (is.null(plot_vars)) idx_freq else match(plot_vars, colnames(Rk))
  plot_idx <- base::union(idx_freq, idx_vars)
  Rk <- as.data.frame(Rk)
  Rk <- Rk[, plot_idx]
  Rk[] <- lapply(Rk, function(x) factor(x, ordered = TRUE, 
                                        levels = seq_len(ncol(l$ranks) + 1), 
                                        labels = c(seq_len(ncol(l$ranks)), 'not sel.')))
  discparcoord(Rk, k = k, 
               name = paste(k, "most frequent tuples of ranks of selected variables"),
               differentiate = FALSE) |>
  # make background transparent
  layout(plot_bgcolor  = "rgba(0,0,0,0)",
         paper_bgcolor = "rgba(0,0,0,0)")
}

#' Plot (individual) selection frequency for each variable
#' 
#' @param l Result list from apply_KFOCI
#' @param plot_freq Only plot variables that have an individual selection frequency of at least min_freq
#' @param plot_vars Plot variables in plot_vars in any case
#' @param title Title for the plot
#' @param subtitle Subtitle for the plot
#' @param caption Caption for the plot
plot_selection_freq <- function(l, plot_freq = 0, plot_vars = NULL,
                                title = NULL, subtitle = NULL, caption = NULL) {
  library(ggplot2)
  s_freq <- data.frame(selection_freq = as.numeric(rowMeans(l$selected)), 
                       variable_label = rownames(l$selected),
                       selected = 0)
  s_freq[l$selected_indices, "selected"] <- 1
  s_freq$selected <- as.factor(s_freq$selected)
  
  # Define elements for plotting
  idx_freq <- which(base::rowMeans(l$selected) >= plot_freq)
  idx_vars <- if (is.null(plot_vars)) idx_freq else match(plot_vars, rownames(l$selected))
  plot_idx <- base::union(idx_freq, idx_vars)
  d <- s_freq[plot_idx, ]
  colors <- if (nrow(d) == sum(as.integer(d$selected == 1))) "#F28E2B" else c("black", "#F28E2B")
  plot_title <- if (is.null(title)) paste("Frequency of individual selected variables across", nrow(l$selected), "repetitions") else title
  if (is.null(subtitle)) {
    if (is.null(plot_vars))
      plot_subtitle <- paste("Variables with selection frequency of at least", round(plot_freq, 2), "are shown")
    else
      plot_subtitle <- paste("Variables with selection frequency of at least", round(plot_freq, 2), "and", paste(plot_vars, collapse = ", "), "are shown")
  } else {
    plot_subtitle <- subtitle
  }
  plot_caption <- if (is.null(caption)) "Frequencies of variables highlighted in orange are selected according to the algorithm from Sec. 2.3 in Kormarksson et al." else caption
  
  ggplot(data = d, 
         mapping = aes(x = selection_freq, y = reorder(variable_label, selection_freq), col = selected)) +
    geom_point() +
    theme_classic() +
    scale_x_continuous(limits = c(0, 1.005)) +
    scale_color_manual(values = colors) +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x = element_line(linewidth = 0.2),
          axis.title.x = element_text(hjust = 1),
          panel.grid.major.y = element_line(linewidth = 0.2, color = "gray70"),
          plot.caption = element_text(color = "gray50"),
          panel.background = element_blank(),
          plot.margin = margin(t = 5.5, r = 12, b = 5.5, l = 5),
          legend.position = "none") +
    labs(x = "Frequency", y = "",
         title = plot_title, subtitle = plot_subtitle, caption = plot_caption)
}

plot_selection_heatmap <- function(l, ...) {
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("dplyr is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required.")
  if (!requireNamespace("ggtext", quietly = TRUE))
    stop("ggtext is required.")
  object <- list(selected = l$selected, stable.variables = l$selected_names)
  class(object) <- c("variable.selections", class(object))
  p <- knockofftools:::plot.variable.selections(object, ...)
  y_labels <- sapply(
    unique(rownames(object$selected)),
    function(x) {
      color <- if (x %in% object$stable.variables) "red" else "black"
        paste0("<span style='color:", color, ";'>", x, "</span>")
    }
  )
  p <- p + ggplot2::scale_y_discrete(labels = y_labels)
  p <- suppressMessages(p + ggplot2::scale_fill_manual(name = "Variable selected",
                                                       values = c("0"="#132B43","1"="#56B1F7"),
                                                       labels = c("0" = "No", "1" = "Yes")))
  p <- p + ggplot2::theme(
    legend.position = "bottom",
    axis.title.x = ggplot2::element_text(hjust = 1, margin = ggplot2::margin(t = 6)),
    axis.title.y = ggplot2::element_text(hjust = 1, vjust = 1, angle = 90,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.y = ggtext::element_markdown()
  )
  p <- p + labs(x = "Repetition", y = "Variable", 
                caption = "Stable selection colored in red")
  p
}