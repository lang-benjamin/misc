#' Identify constant (or near-constant) columns #'
#' @param d Data frame or matrix
#' @param tol Numeric tolerance used only for numeric/integer types to treat
#'   a column as (near-)constant when var(x, na.rm = TRUE) < tol. Default 0
#'   means strictly constant (ignoring NAs).
#' @return Integer vector of column indices that are (near-)constant.
constant_columns <- function(d, tol = 0) {
  if (!is.data.frame(d)) d <- as.data.frame(d)
  
  is_const <- function(x) {
    # Treat all-NA as constant across types
    if (all(is.na(x))) return(TRUE)
    
    # Count unique non-NA values
    uniq_non_na <- function(z) {
      u <- unique(z)
      # Drop NA from uniques without coercing types
      if (anyNA(u)) u <- u[!is.na(u)]
      length(u)
    }
    
    # Numeric-like types
    if (is.numeric(x) || is.integer(x) || is.logical(x) || inherits(x, c("Date", "POSIXct", "POSIXt"))) {
      # strictly constant if only one unique non-NA value
      if (uniq_non_na(x) <= 1) return(TRUE)
      # Optional: near-constant using variance tolerance
      if (tol > 0) return(stats::var(as.numeric(x), na.rm = TRUE) < tol)
      return(FALSE)
    }
    
    # Factor/character/ordered
    if (is.factor(x) || is.character(x) || is.ordered(x)) {
      return(uniq_non_na(x) <= 1)
    }
    
    # Fallback: try numeric coercion for novel classes
    suppressWarnings({
      xn <- as.numeric(x)
    })
    if (!all(is.na(xn))) {
      if (length(unique(stats::na.omit(xn))) <= 1) return(TRUE)
      if (tol > 0) return(stats::var(xn, na.rm = TRUE) < tol)
    }
    FALSE
  }
  
  idx <- which(vapply(d, is_const, logical(1)))
  unname(idx)
}

#  Part of the following code was inspired by qeML::qeFOCI by Norm Matloff
#' Simple pre-processing of a data frame into X and Y #'
#' @description Converts mixed-type predictors to a numeric design matrix with
#'   optional scaling, handles binary/ordinal/unordered factors, drops constant
#'   columns, and encodes the response. Multivariate responses are supported.
#'
#' @param d A data frame containing predictors and response; response can be multivariate.
#' @param y_name Character vector with the response variable name(s) in `d` (or
#'   integer positions, advanced use).
#' @param y_yes_level If `y` is a factor with two levels, the level treated as
#'   "1" (Yes). Otherwise ignored.
#' @param remove_na Logical. If `TRUE`, drop rows with any NA in X or Y.
#' @param ordered_coding How to code *ordered* factors in X: one of
#'   `"dummy"`, `"one-hot"`, `"integer"`, `"none"`.
#'   - `"dummy"` and `"one-hot"` coerce ordered factors to unordered factors so they
#'     will be expanded by `model.matrix()` later.
#'   - `"integer"` maps the order to integers starting at 0.
#'   - `"none"` keeps them as ordered and they will be passed through as-is.
#' @param unordered_coding How to code *unordered* factors in X: one of
#'   `"dummy"` (k-1 dummies via treatment contrasts) or `"one-hot"` (k dummies).
#' @param scale Scaling for X: one of `"none"`, `"mean_2sd"`, `"mean_sd_all"`.
#'   - `"mean_2sd"`: center and divide by 2*SD for non-binary columns only.
#'   - `"mean_sd_all"`: standard z-scoring for all columns.
#'   - `"none"`: return unscaled X.
#' @return A list with elements `X` (numeric matrix), `X_unscaled` (before scaling), and `Y`.
prep_data <- function(d, y_name, y_yes_level = NULL, remove_na = FALSE,
                      ordered_coding = c("dummy", "one-hot", "integer", "none"),
                      unordered_coding = c("dummy", "one-hot"),
                      scale = c("none", "mean_2sd", "mean_sd_all")) {
  # ---- Argument checks ----
  if (!is.data.frame(d)) stop("d should be a data frame.")
  if (is.character(y_name) && !all(y_name %in% names(d)))
    stop("y_name should match column names in d.")
  if (!is.logical(remove_na) || length(remove_na) != 1)
    stop("remove_na should be a single logical (TRUE/FALSE).")
  
  # Positions of response columns (allow integer indices)
  idx_y <- if (is.character(y_name)) match(y_name, names(d)) else y_name
  
  # ---- Coerce some types early ----
  # characters -> factors; logical -> integer
  d[] <- lapply(d, function(x) {
    if (is.character(x)) return(as.factor(x))
    if (is.logical(x)) return(as.integer(x))
    x
  })
  
  # Split X and Y
  y <- d[, idx_y, drop = TRUE]
  X <- d[, -idx_y, drop = FALSE]
  
  # Optional NA removal across X and Y
  if (isTRUE(remove_na)) {
    ok <- stats::complete.cases(X, y)
    X <- X[ok, , drop = FALSE]
    y <- if (length(idx_y) > 1) y[ok, , drop = FALSE] else y[ok]
  }
  
  # ---- Handle Y encoding ----
  if (length(idx_y) == 1) {
    # 1D response
    if (!is.numeric(y)) {
      if (is.factor(y) && nlevels(y) == 2) {
        if (!is.null(y_yes_level)) {
          if (!y_yes_level %in% levels(y))
            stop("y_yes_level not found in factor levels of y.")
          y <- as.integer(y == y_yes_level)
        } else {
          message("y converted to {0,1} with '", levels(y)[1], "' -> 0.")
          y <- as.integer(y) - 1
        }
      } else if (is.factor(y)) {
        message("y factor converted to integers starting at 0.")
        y <- as.integer(y) - 1
      }
    }
  } else {
    # Multivariate response
    if (is.data.frame(y)) {
      # Special case: exactly one binary factor component
      idx_binary_y <- vapply(y, function(x) is.factor(x) && nlevels(x) == 2, logical(1))
      if (sum(idx_binary_y) == 1) {
        if (is.null(y_yes_level)) {
          message("Binary factor component in y converted to {0,1} with first level -> 0.")
          y[, idx_binary_y] <- as.integer(y[, idx_binary_y, drop = TRUE]) - 1
        } else {
          levs <- levels(y[, idx_binary_y, drop = TRUE])
          if (!y_yes_level %in% levs)
            stop("y_yes_level not found in the binary factor component of y.")
          y[, idx_binary_y] <- as.integer(y[, idx_binary_y, drop = TRUE] == y_yes_level)
        }
      }
      # Convert remaining factors to integers starting at 0
      y[] <- lapply(y, function(x) if (is.factor(x)) as.integer(x) - 1 else x)
    }
  }
  
  # ---- Handle X encoding ----
  ordered_coding <- match.arg(ordered_coding)
  unordered_coding <- match.arg(unordered_coding)
  
  # Process ordered factors according to option
  X[] <- lapply(X, function(z) {
    if (is.ordered(z)) {
      switch(
        ordered_coding,
        "dummy"   = factor(z, ordered = FALSE),
        "one-hot" = factor(z, ordered = FALSE),
        "integer" = as.integer(z) - 1,
        "none"    = z
      )
    } else {
      z
    }
  })
  
  # Drop constant columns
  idx_const <- constant_columns(X)
  if (length(idx_const) > 0) {
    X <- X[, setdiff(seq_len(ncol(X)), idx_const), drop = FALSE]
    warning(sprintf(
      "Constant column(s) removed: %s",
      paste(idx_const, collapse = ", ")
    ))
  }
  
  # Expand unordered factors with model.matrix
  if (any(vapply(X, is.factor, logical(1)))) {
    old_na <- getOption("na.action")
    on.exit(options(na.action = old_na), add = TRUE)
    options(na.action = "na.pass")
    
    if (unordered_coding == "dummy") {
      # treatment contrasts -> k-1 dummies, no intercept
      X <- stats::model.matrix(~ . , X)
      # Drop intercept if present
      if (colnames(X)[1] == "(Intercept)") X <- X[, -1, drop = FALSE]
    } else {
      # one-hot: k dummies
      fac_idx <- vapply(X, is.factor, logical(1))
      contr <- lapply(X[, fac_idx, drop = FALSE], stats::contrasts, contrasts = FALSE)
      X <- stats::model.matrix(~ 0 + . , X, contrasts.arg = contr)
    }
  }
  
  # Ensure a numeric matrix result
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  X_unscaled <- X
  
  # ---- Optional scaling ----
  scale <- match.arg(scale)
  if (scale == "none") return(list(X = X_unscaled, X_unscaled = X_unscaled, Y = y))
  
  # Identify non-binary columns robustly (ignore NAs)
  is_non_binary <- function(z) {
    u <- unique(z)
    if (anyNA(u)) u <- u[!is.na(u)]
    length(u) != 2
  }
  idx_not_binary <- vapply(seq_len(ncol(X)), function(j) is_non_binary(X[, j]), logical(1))
  
  if (scale == "mean_2sd") {
    if (any(idx_not_binary)) {
      sds <- apply(X[, idx_not_binary, drop = FALSE], 2, stats::sd, na.rm = TRUE)
      # guard against zero SD
      sds[!is.finite(sds) | sds == 0] <- 1
      X[, idx_not_binary] <- scale(
        X[, idx_not_binary, drop = FALSE],
        center = TRUE,
        scale  = 2 * sds
      )
    }
  } else if (scale == "mean_sd_all") {
    X <- scale(X, center = TRUE, scale = TRUE)
  }
  
  list(X = X, X_unscaled = X_unscaled, Y = y)
}
