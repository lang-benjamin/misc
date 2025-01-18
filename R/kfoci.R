library(KPC)
if (!require(knockofftools)) 
  devtools::install_github("Novartis/knockofftools")

new_env <- new.env()
devtools::source_url("https://github.com/lang-benjamin/misc/blob/main/R/fun_eda.R?raw=true", local = new_env)
constant_columns <- new_env$constant_columns
prep_data <- new_env$prep_data
rm(new_env)

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
#' @param ordered_coding How should ordinal factor variables be coded? "integer" converts the ordinal variable into an integer-valued variable (the default), "dummy" does dummy coding,, "one-hot" does one-hot encoding and "none" leaves the factors as they are.
#' @param unordered_coding How should non-ordinal factor variables be coded? "dummy" does dummy coding, "one-hot" does one-hot encoding.
#' @param Knn Same as in KPC::KFOCI but defined as function of the sample size n. Constant values are allowed.
#' @param numCores Same as numCores from KPC::KFOCI.
#' @param R Positive integer. Number of times KFOCI is called repeatedly, see details.
#' @param subsampling Logical. If TRUE perform random subsampling to assess stability of the selection. If FALSE (default) random subsampling is not done.
#'
#' @details If R > 1 and subsampling = FALSE, KFOCI is called R times to investigate the effect of random breaking of ties for the kNN graphs. 
#'    If R > 1 and subsampling = TRUE, KFOCI is called R times to investigate the stability of the selection via random subsampling of size 0.632*n.
#'    
#' @return A list. If R = 1, then selected_indices is the return value from KFOCI and selected_names are the corresponding variable names in d.
#'    If R > 1, 'selected_indices' is the stable selection of variables according to the algorithm in Section 2.3 in Kormaksson et al. (https://doi.org/10.1002/sim.8955) and 'selected_names' are the corresponding variable names in d.
#'    The list element 'selections' contains a matrix indicating whether each of the variables was selected (1) or not (0) in each of the R repetitions, and 'ranks' contains the rank of each variable as obtained by KFOCI for each of the R runs.
#'    The element 'new_data' contains the data set after factor variables have been transformed but without scaling, restricted to the selected variables.
apply_KFOCI <- function(d, y_name, y_yes_level = NULL, 
                        scale = c("mean_2sd", "mean_sd_all", "none"),
                        ordered_coding = c("integer", "dummy", "one-hot", "none"),
                        unordered_coding = c("dummy", "one-hot"),
                        Knn = function(n) max(min(ceiling(n/20), 20), 2),
                        numCores = parallel::detectCores(), R = 1, 
                        subsampling = FALSE) {
  # Argument checks
  if (!is.data.frame(d))
    stop("d should be a data frame.")
  if (is.character(y_name) && !all(y_name %in% names(d)))
    stop("y_name should be a string or a vector of strings that match the response variable(s) in d.")
  if (is.numeric(Knn)) {
    if (floor(Knn) != Knn || Knn <= 0)
      stop("Knn should be a positive integer.")
    const <- Knn
    Knn <- function(n) const
  } else if (!is.function(Knn)) {
    stop("Knn should be a function of the sample size n")
  }
  if (!is.logical(subsampling))
    stop("subsampling should be logical.")
  if (floor(R) != R || R <= 0)
    stop("R should be a positive integer.")
  
  ordered_coding <- match.arg(ordered_coding)
  unordered_coding <- match.arg(unordered_coding)
  scale <- match.arg(scale)
  XY <- prep_data(d = d, y_name = y_name, y_yes_level = y_yes_level, 
                  remove_na = TRUE, ordered_coding = ordered_coding, 
                  unordered_coding = unordered_coding, scale = scale)
  X <- XY$X
  X_unscaled <- if (scale == "none") X else XY$X_unscaled
  Y <- XY$Y
  
  # Prepare for KFOCI
  k_Knn <- Knn(nrow(X))
  if (floor(k_Knn) != k_Knn || k_Knn <= 0)
    stop("Knn should be a positive integer.")
  # Set package default for bandwidth
  bw <- stats::median(stats::dist(Y))
  # If, however, Y is 1-dimensional and binary, then the median (of pairwise
  # differences) is not appropriate (because of heavy ties in Y).
  # In this case and in other cases where the median is zero, use mean instead.
  idx_y <- if (is.character(y_name)) match(y_name, names(d)) else y_name
  if ((length(idx_y) == 1 && length(unique(Y)) == 2) || bw == 0)
    bw <- base::mean(stats::dist(Y))
  
  if (R == 1) {
    # Selection vector that identifies if a variable was selected or not
    S <- vector("integer", ncol(X))
    # Vector of ranks that stores rank of each variable
    # (initialize with dummy value ncol(X) + 1 as rank, corresponding to 'not selected')
    Rk <- rep.int(ncol(X) + 1, ncol(X))
    names(S) <- names(Rk) <- colnames(X)
    selected <- KFOCI(Y = Y, X = X, k = kernlab::rbfdot(1 / (2 * bw^2)), 
                      Knn = k_Knn, numCores = numCores)
    if (length(selected) == 1 && selected == 0L)
      return(list(selected_indices = integer(0),
                  selected_names = NULL,
                  selections = S,
                  ranks = Rk,
                  new_data = data.frame()))
    S[selected] <- 1
    for (i in seq_along(selected)) {
      Rk[which(colnames(X) == colnames(X)[selected[i]])] <- i
    }
    new_data <- as.data.frame(cbind(X_unscaled[, selected], Y))
    colnames(new_data)[1:(ncol(new_data) - length(idx_y))] <- colnames(X_unscaled)[selected]
    colnames(new_data)[(ncol(new_data) - length(idx_y) + 1):ncol(new_data)] <- colnames(d)[idx_y]
    return(list(selected_indices = selected, 
                selected_names = colnames(X)[selected],
                selections = S,
                ranks = Rk,
                new_data = new_data))
  } else {
    S <- matrix(0L, nrow = ncol(X), ncol = R)
    Rk <- matrix(ncol(X) + 1, nrow = ncol(X), ncol = R)
    rownames(S) <- rownames(Rk) <- colnames(X)
    colnames(S) <- colnames(Rk) <- paste0("rep_", seq_len(R))
    for (j in 1:R) {
      if (subsampling) {
        # Uncertainty will be explored by performing random subsampling
        i_sub <- sample(seq_len(nrow(X)), round(0.632 * nrow(X)), replace = FALSE)
        X_sub <- X[i_sub, ]
        Y_sub <- if (length(idx_y) > 1) Y[i_sub, ] else Y[i_sub]
        k_Knn <- Knn(nrow(X_sub))
        if (floor(k_Knn) != k_Knn || k_Knn <= 0)
          stop("Knn should be a positive integer.")
        bw <- stats::median(stats::dist(Y_sub))
        if ((length(idx_y) == 1 && length(unique(Y_sub)) == 2) || bw == 0)
          bw <- base::mean(stats::dist(Y_sub))
        selected_vars <- KFOCI(Y = Y_sub, X = X_sub, 
                               k = kernlab::rbfdot(1 / (2 * bw^2)),
                               Knn = k_Knn, numCores = numCores)
      } else {
        # Explore random breaking of ties by calling KFOCI R times (on same data)
        selected_vars <- KFOCI(Y = Y, X = X, 
                               k = kernlab::rbfdot(1 / (2 * bw^2)),
                               Knn = k_Knn, numCores = numCores)
      }
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
      selected <- multi_select(S)
      selected_names <- colnames(X)[selected]
    }
    if (length(selected) == 0 && typeof(selected) == "integer") {
      new_data = data.frame()
    } else {
      new_data <- as.data.frame(cbind(X_unscaled[, selected], Y))
      colnames(new_data)[1:(ncol(new_data) - length(idx_y))] <- colnames(X_unscaled)[selected]
      colnames(new_data)[(ncol(new_data) - length(idx_y) + 1):ncol(new_data)] <- colnames(d)[idx_y]
    }
    return(list(selected_indices = selected, 
                selected_names = selected_names, 
                selections = t(S), 
                ranks = t(Rk),
                new_data = new_data))
  }
}

#' Plot most frequent tuples of ranks from selected variables
#' 
#' @param l Result list from apply_KFOCI
#' @param k Number of most frequent tuples being plotted
#' @param plot_freq Only plot variables that have an individual selection frequency of at least min_freq
#' @param plot_vars Plot variables in plot_vars in any case
plot_rank_tuples <- function(l, k = 5, plot_freq = 0, plot_vars = NULL) {
  Rk <- l$ranks
  # Check if all rows of the rank matrix are the same
  if (all(duplicated(Rk)[-1]))
    stop("There is no variation in the tuples of selected variables across the multiple KFOCI calls.")
  library(cdparcoord)
  idx_freq <- which(base::colMeans(l$selections) >= plot_freq)
  idx_vars <- match(plot_vars, colnames(Rk))
  plot_idx <- base::intersect(idx_freq, idx_vars)
  Rk <- as.data.frame(Rk)
  Rk <- Rk[, plot_idx]
  Rk[] <- lapply(Rk, function(x) factor(x, ordered = TRUE, 
                                        levels = seq_len(ncol(l$ranks) + 1), 
                                        labels = c(seq_len(ncol(l$ranks)), 'ns')))
  discparcoord(Rk, k = k, 
               name = paste(k, "most frequent tuples of ranks of selected variables"))
}

plot_rank_freq <- function(l) {
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  d <- pivot_longer(as.data.frame(l$ranks), cols = everything(),
                    names_to = "Variable", values_to = "Rank_Value") %>%
    mutate(Rank_Group = factor(Rank_Value, ordered = TRUE, 
                               levels = seq_len(ncol(l$ranks) + 1),
                               labels = c(seq_len(ncol(l$ranks)), 'ns'))) %>%
    count(Variable, Rank_Group) %>% 
    group_by(Variable) %>% 
    mutate(Percentage = proportions(n)) %>%
    mutate(Rank_Value = as.numeric(ifelse(Rank_Group == "ns", ncol(l$ranks) + 1, as.character(Rank_Group)))) %>%
    mutate(Weight = ncol(l$ranks) + 1 - Rank_Value) %>%
    mutate(Weighted_Avg = sum(Weight * Percentage, na.rm = TRUE)) %>% 
    ungroup() %>%
    arrange(desc(Weighted_Avg))
  
  ggplot(d, aes(x = reorder(Variable, Weighted_Avg), y = Percentage)) +
    geom_hline(yintercept = seq(0.1, 0.9, 0.1), linetype = "dashed", color = "gray75", linewidth = 0.2) +
    geom_bar(stat = "identity", fill = NA, color = "black", linewidth = 0.2, width = 0.3) +
    geom_text(aes(label = ifelse(Rank_Group == "ns", paste0(Rank_Group, " (", round(Percentage*100, 1), "%)"), Rank_Group)), 
                  position = position_stack(vjust = 0.5), size = 0.35 * 8, color = "gray30") +
    scale_y_continuous(labels = scales::percent_format(),
                       breaks = seq(0, 1, 0.1)) +
    coord_flip() +
    theme_classic() +
    theme(legend.position = "none") +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x = element_line(linewidth = 0.2),
          axis.title.x = element_text(hjust = 1, vjust = -1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.caption = element_text(color = "gray50"),
          panel.background = element_blank(),
          plot.margin = margin(t = 5.5, r = 12, b = 5.5, l = 5),
          legend.position = "none") +
    labs(x = "", y = "Cumulative percentage", fill = "Rank_Group")
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
  s_freq <- data.frame(selection_freq = as.numeric(colMeans(l$selections)), 
                       variable_label = colnames(l$selections),
                       selected = 0)
  s_freq[l$selected_indices, "selected"] <- 1
  s_freq$selected <- as.factor(s_freq$selected)
  
  # Define elements for plotting
  idx_freq <- which(base::colMeans(l$selections) >= plot_freq)
  idx_vars <- match(plot_vars, colnames(l$selections))
  plot_idx <- base::intersect(idx_freq, idx_vars)
  d <- s_freq[plot_idx, ]
  colors <- if (nrow(d) == sum(as.integer(d$selected == 1))) "#F28E2B" else c("black", "#F28E2B")
  plot_title <- if (is.null(title)) paste("Frequency of individual selected variables across", nrow(l$selections), "repetitions") else title
  plot_subtitle <- if (is.null(subtitle)) paste("Variables with selection frequency of at least", round(plot_freq, 2), "are shown") else subtitle
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
