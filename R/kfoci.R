library(KPC)
if (!requireNamespace("knockofftools", quietly = TRUE)) 
  devtools::install_github("Novartis/knockofftools")
if (!requireNamespace("energy", quietly = TRUE)) 
  install.packages("energy")

devtools::source_url("https://github.com/lang-benjamin/misc/blob/main/R/prep_data.R?raw=true")

# Part of the following code was inspired by qeML::qeFOCI by Norm Matloff
#' Apply KFOCI with preprocessing, optional replication, and first-variable test #'
#' Runs KFOCI on preprocessed predictors, optionally repeating the run R times
#' to assess tie-breaking variability and consolidate selections.
#'
#' @param X A matrix or data frame of candidate predictors, size n x p.
#' @param Y A vector/matrix/data frame response with n rows (univariate or multivariate).
#' @param Z Integer vector of indices into the columns of X to pre-condition on,
#'   or NULL for no pre-conditioning. Indices refer to columns of X **before** preprocessing/encoding.
#' @param y_yes_level If `y` is a factor with two levels, the level treated as
#'   "1" (Yes). Otherwise ignored.
#' @param scale Scaling for X: one of `"none"`, `"mean_2sd"`, `"mean_sd_all"`.
#'   - `"mean_2sd"`: center and divide by 2*SD for non-binary columns only.
#'   - `"mean_sd_all"`: standard z-scoring for all columns.
#'   - `"none"`: return unscaled X.
#' @param ordered_coding How to code *ordered* factors in X: one of
#'   `"dummy"`, `"one-hot"`, `"integer"`, `"none"`.
#'   - `"dummy"` and `"one-hot"` coerce ordered factors to unordered factors so they
#'     will be expanded by `model.matrix()` later.
#'   - `"integer"` maps the order to integers starting at 0.
#'   - `"none"` keeps them as ordered and they will be passed through as-is.
#' @param unordered_coding How to code *unordered* factors in X: one of
#'   `"dummy"` (k-1 dummies via treatment contrasts) or `"one-hot"` (k dummies).
#' @param R Integer (>=1), number of independent repetitions of KFOCI.
#' @param first_test Logical; if TRUE and Z is NULL, test the first selected variable via energy::indep.test.
#' @param first_test_alpha Numeric; significance level for the first-variable test (default 0.10).
#' @param first_test_R Integer; number of permutations for the test (default 500).
#' @param ... Further arguments forwarded to KFOCI (e.g., k, Knn, num_features,
#'   stop, numCores, verbose).
#'
#' @return A list with components:
#' * selected_indices: integer indices **within the non-pre-conditioned encoded columns** (stable set when R>1).
#' * selected_names: column names corresponding to selected_indices (non-pre-conditioned only).
#' * selected: matrix of 0/1 selections per repetition (always a matrix, even for R=1).
#' * ranks: matrix of within-rep ranks (number of non-pre-conditioned columns + 1 for unselected).
#'
#' @details Pre-conditioned columns (specified by Z) are **always** included as conditioning variables
#' in each step and are not part of the selection output (they are not re-selected).
#' If ranks are identical across repetitions, the shared ordering is returned. Otherwise,
#' a stable selection is formed with knockofftools::multi_select() on the selection indicators.
#' The first-variable independence test is implemented here (not in KFOCI) and applied only
#'  when no pre-conditioned variables are present.
#'
#' @examples
#' set.seed(1)
#' n <- 200; p <- 10
#' X <- matrix(rnorm(n*p), n, p); colnames(X) <- paste0("x", 1:p)
#' Y <- X[,1] - X[,2] + rnorm(n)
#' out <- apply_KFOCI(X, Y, R = 3, Knn = 5, first_test = TRUE, num_features = 5)
#' out$selected_names
apply_KFOCI <- function(X, Y, Z = NULL,
                        y_yes_level = NULL,
                        scale = c("mean_sd_all", "mean_2sd", "none"),
                        ordered_coding = c("integer", "one-hot", "dummy", "none"),
                        unordered_coding = c("one-hot", "dummy"),
                        first_test = TRUE,
                        first_test_alpha = 0.10,
                        first_test_R = 500,
                        R = 1L, ...) {
  X <- as.data.frame(X)
  if (is.null(colnames(X))) {
    w <- nchar(ncol(X))
    colnames(X) <- sprintf(paste0("X%0", w, "d"), seq_len(ncol(X)))
  }
  if (!is.null(Z)) {
    if (!is.numeric(Z)) stop("Z must be NULL or an integer vector of column indices of X.")
    Z <- unique(as.integer(Z))
    if (length(Z) && (min(Z) < 1 || max(Z) > ncol(X))) stop("Z indices out of bounds for X.")
  }
  
  scale <- match.arg(scale)
  ordered_coding  <- match.arg(ordered_coding)
  unordered_coding <- match.arg(unordered_coding)
  if (floor(R) != R || R <= 0) stop("R should be a positive integer.")
  
  # Tag original columns so we can map Z indices after encoding
  p0 <- ncol(X)
  id_width <- nchar(p0)
  x_ids <- sprintf(paste0("Xv%0", id_width, "d__"), seq_len(p0))
  colnames(X) <- paste0(x_ids, colnames(X))
  
  # Build data frame for pre-processing
  if (is.vector(Y)) Y <- as.matrix(Y)
  if (is.null(colnames(Y))) colnames(Y) <- if (ncol(Y) == 1) "Y" else paste0("Y", seq_len(ncol(Y)))
  d_for_prep <- cbind(X, as.data.frame(Y))
  y_names <- colnames(Y)
  
  XY <- prep_data(
    d = d_for_prep,
    y_name = y_names,
    y_yes_level = y_yes_level,
    remove_na = TRUE,
    ordered_coding = ordered_coding,
    unordered_coding = unordered_coding,
    scale = scale
  )
  
  X_enc_all <- XY$X
  Y_prep    <- XY$Y
  
  # Bandwidth fallback (on preprocessed Y)
  dY <- stats::dist(Y_prep)
  bw <- stats::median(dY)
  tab <- table(as.numeric(dY))
  heavy <- (max(tab) / length(dY)) > 0.5
  if (bw == 0 || heavy) {
    bw <- base::mean(dY)
  }
  if (!is.finite(bw) || bw <= 0) bw <- 1
  k <- kernlab::rbfdot(1 / (2 * bw^2))
  
  # Map forced indices to encoded columns
  forced_cols <- integer(0)
  if (!is.null(Z) && length(Z)) {
    pat <- paste0("^(?:", paste0(x_ids[Z], collapse = "|"), ")")
    forced_cols <- grep(pat, colnames(X_enc_all), perl = TRUE)
  }
  
  all_cols <- seq_len(ncol(X_enc_all))
  free_cols <- setdiff(all_cols, forced_cols)
  p_free <- length(free_cols)
  if (p_free == 0L) {
    return(list(
      selected_indices = integer(0),
      selected_names   = character(0),
      selected         = matrix(0L, nrow = 0, ncol = R),
      ranks            = matrix(0L, nrow = 0, ncol = R)
    ))
  }
  
  # Clean, user-facing names without the internal Xv..__ prefix
  clean_names_all  <- sub("^Xv\\d+__", "", colnames(X_enc_all))
  clean_free_names <- clean_names_all[free_cols]
  
  S  <- matrix(0L, nrow = p_free, ncol = R)
  Rk <- matrix(p_free + 1L, nrow = p_free, ncol = R)
  rownames(S)  <- clean_free_names
  rownames(Rk) <- clean_free_names
  if (R > 1L) {
    colnames(S)  <- paste0("rep_", seq_len(R))
    colnames(Rk) <- paste0("rep_", seq_len(R))
  } else {
    colnames(S)  <- NULL
    colnames(Rk) <- NULL
  }
  
  for (j in seq_len(R)) {
    sel_abs <- KFOCI(Y = Y_prep, X = X_enc_all, Z = forced_cols, k = k, ...)
    
    if (isTRUE(first_test) && length(forced_cols) == 0 && length(sel_abs) > 0) {
      if (!requireNamespace("energy", quietly = TRUE))
        stop("Package 'energy' is required for first_test; install 'energy'.")
      first_idx <- sel_abs[1]
      pval_first <- energy::indep.test(x = X_enc_all[, first_idx, drop = FALSE],
                                       y = Y_prep,
                                       R = first_test_R)$p.value
      if (!is.finite(pval_first) || is.na(pval_first)) pval_first <- 1
      if (pval_first > first_test_alpha) {
        next
      }
    }
    
    if (length(sel_abs)) {
      loc <- match(sel_abs, free_cols)
      loc <- loc[!is.na(loc)]
      if (length(loc)) {
        S[loc, j]  <- 1L
        Rk[loc, j] <- seq_along(loc)  # rank = forward selection order
      }
    }
  }
  
  # Consolidate across repetitions
  if (R == 1L) {
    sel_idx <- which(S[, 1L, drop = TRUE] == 1L)
    if (length(sel_idx)) {
      ord <- order(Rk[sel_idx, 1L, drop = TRUE])
      sel_idx <- sel_idx[ord]
    }
    sel_names <- rownames(S)[sel_idx]
  } else {
    same_ranks <- all(duplicated(t(Rk))[-1])
    if (same_ranks) {
      ord <- order(Rk[, 1L])
      sel_idx <- ord[Rk[ord, 1L] < (p_free + 1L)]
    } else {
      if (!requireNamespace("knockofftools", quietly = TRUE))
        stop("Package 'knockofftools' is required for stable multi-run selection.")
      sel_idx <- knockofftools::multi_select(S)
      sel_idx <- sort(sel_idx)  # return in original X (encoded free-cols) order
    }
    sel_names <- rownames(S)[sel_idx]
  }
  
  list(
    selected_indices = sel_idx,
    selected_names   = sel_names,
    selected         = S,
    ranks            = Rk
  )
}

#' Plot most frequent tuples of ranks from selected variables #'
#' @param l Result from `apply_KFOCI()`.
#' @param k Number of most frequent tuples being plotted.
#' @param plot_freq Only plot variables that have a selection frequency >= this threshold.
#' @param plot_vars Always include these variables (by name) regardless of frequency.
#' @export
plot_rank_tuples <- function(l, k = 5, plot_freq = 0, plot_vars = NULL) {
  if (is.vector(l$ranks)) {
    # R=1 convenience: coerce to 1-column matrix
    l$ranks <- matrix(l$ranks, ncol = 1L, dimnames = list(names(l$ranks), "rep_1"))
  }
  if (is.vector(l$selected)) {
    l$selected <- matrix(l$selected, ncol = 1L, dimnames = list(names(l$selected), "rep_1"))
  }
  
  Rk <- t(l$ranks)
  if (ncol(Rk) < 2L) stop("Need at least 2 variables to form rank tuples.")
  if (all(duplicated(Rk)[-1])) stop("No variation in tuples of ranks across repetitions.")
  
  if (!requireNamespace("cdparcoord", quietly = TRUE))
    stop("Package 'cdparcoord' is required for this plot.")
  
  idx_freq <- which(base::rowMeans(l$selected) >= plot_freq)
  idx_vars <- if (is.null(plot_vars)) idx_freq else match(plot_vars, colnames(Rk))
  plot_idx <- base::union(idx_freq, idx_vars)
  
  Rk <- as.data.frame(Rk)
  Rk <- Rk[, plot_idx, drop = FALSE]
  Rk[] <- lapply(Rk, function(x) factor(x, ordered = TRUE,
                                        levels = seq_len(ncol(Rk) + 1),
                                        labels = c(seq_len(ncol(Rk)), 'not sel.')))
  cdparcoord::discparcoord(Rk, k = k,
                           name = paste(k, "most frequent tuples of ranks of selected variables"),
                           differentiate = FALSE) |>
    layout(plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
}

#' Plot (individual) selection frequency for each variable #'
#' @param l Result from `apply_KFOCI()`.
#' @param plot_freq Only plot variables with selection frequency >= this value.
#' @param plot_vars Always include these variables.
#' @param title,subtitle,caption Optional strings.
#' @export
plot_selection_freq <- function(l, plot_freq = 0, plot_vars = NULL,
                                title = NULL, subtitle = NULL, caption = NULL) {
  if (is.vector(l$selected)) {
    l$selected <- matrix(l$selected, ncol = 1L, dimnames = list(names(l$selected), "rep_1"))
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required.")
  
  s_freq <- data.frame(selection_freq = as.numeric(rowMeans(l$selected)),
                       variable_label = rownames(l$selected),
                       selected = 0)
  if (!is.null(l$selected_indices) && length(l$selected_indices) > 0)
    s_freq[l$selected_indices, "selected"] <- 1
  s_freq$selected <- as.factor(s_freq$selected)
  
  idx_freq <- which(base::rowMeans(l$selected) >= plot_freq)
  idx_vars <- if (is.null(plot_vars)) idx_freq else match(plot_vars, rownames(l$selected))
  plot_idx <- base::union(idx_freq, idx_vars)
  d <- s_freq[plot_idx, , drop = FALSE]
  
  colors <- if (nrow(d) == sum(as.integer(d$selected == 1))) "#F28E2B" else c("black", "#F28E2B")
  plot_title <- if (is.null(title)) paste("Frequency of individual selected variables across", ncol(l$selected), "repetitions") else title
  if (is.null(subtitle)) {
    if (is.null(plot_vars)) plot_subtitle <- paste("Shown if frequency >=", round(plot_freq, 2))
    else plot_subtitle <- paste("Shown if frequency >=", round(plot_freq, 2), "and:", paste(plot_vars, collapse = ", "))
  } else plot_subtitle <- subtitle
  plot_caption <- if (is.null(caption)) "Orange points are in the stable selection (Kormaksson et al., Sec. 2.3)." else caption
  
  ggplot2::ggplot(d, ggplot2::aes(x = selection_freq, y = stats::reorder(variable_label, selection_freq), col = selected)) +
    ggplot2::geom_point() +
    ggplot2::theme_classic() +
    ggplot2::scale_x_continuous(limits = c(0, 1.005)) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(linewidth = 0.2),
      axis.title.x = ggplot2::element_text(hjust = 1),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, color = "gray70"),
      plot.caption = ggplot2::element_text(color = "gray50"),
      panel.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 5.5, r = 12, b = 5.5, l = 5),
      legend.position = "none"
    ) +
    ggplot2::labs(x = "Frequency", y = "", title = plot_title, subtitle = plot_subtitle, caption = plot_caption) }

#' Selection heatmap across repetitions (highlights stable selection) #'
#' @param l Result from `apply_KFOCI()`.
#' @param ... Passed to `knockofftools::plot.variable.selections`.
#' @export
plot_selection_heatmap <- function(l, ...) {
  sel <- l$selected
  if (is.vector(sel)) {
    sel <- matrix(sel, ncol = 1L, dimnames = list(names(sel), NULL))
  }
  if (!requireNamespace("knockofftools", quietly = TRUE)) stop("knockofftools is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required.")
  if (!requireNamespace("ggtext", quietly = TRUE)) stop("ggtext is required.")
  
  object <- list(selected = sel, stable.variables = l$selected_names)
  class(object) <- c("variable.selections", class(object))
  
  p <- suppressWarnings(plot(object, ...))
  
  lab_fun <- function(vals) {
    ifelse(vals %in% object$stable.variables,
           paste0("<span style='color:red'>", vals, "</span>"),
           vals)
  }
  p <- p + ggplot2::scale_y_discrete(labels = lab_fun)
  p <- suppressMessages(
    p + ggplot2::scale_fill_manual(
      name   = "Variable selected",
      values = c("0" = "#132B43", "1" = "#56B1F7"),
      labels = c("0" = "No", "1" = "Yes")
    )
  )
  p <- p + ggplot2::theme_minimal() +   # base theme
    ggplot2::theme(
      legend.position = "bottom",
      axis.title.x = ggplot2::element_text(hjust = 1, margin = ggplot2::margin(t = 6)),
      axis.title.y = ggplot2::element_text(hjust = 1, vjust = 1, angle = 90,
                                           margin = ggplot2::margin(r = 10)),
      axis.text.y  = ggtext::element_markdown()
    )
  p + ggplot2::labs(x = "Repetition", y = "Variable",
                    caption = "Stable selection colored in red")
}
