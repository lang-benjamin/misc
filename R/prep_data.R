#' Identify constant columns of a matrix
#' 
#' @param m Matrix
#' 
#' @return Indices of constant columns
constant_columns <- function(m) {
  var_na <- function(x) {
    if (all(is.na(x)) || length(na.omit(x)) == 1) return(0)
    var(x, na.rm = TRUE) 
  }
  which(apply(m, 2, var_na) < 1e-05)
}

# Part of the following code was inspired by qeML::qeFOCI by Norm Matloff

#' Simple pre-processing of a data frame into X and Y
#' 
#' @description Checks and possibly converts variables of different types to numeric variables and possibly scales them.
#'
#' @param d A data frame containing predictors and response; response can be multivariate.
#' @param y_name The variable name(s) in d that represents the response; can be of length > 1.
#' @param y_yes_level If y is a factor with two levels, y_yes_level is the y value that represents 'Yes', to be coded 1 rather than 0
#' @param remove_na Logical. Remove NAs or keep them?
#' @param ordered_coding How should ordinal factor variables be coded? "dummy" does dummy coding, "one-hot" does one-hot encoding, "integer" converts the ordinal variable into an integer-valued variable, "none" does not do anything and keeps the variable as is.
#' @param unordered_coding How should unordered factor variables be coded? "dummy" or "one-hot" 
#' @param scale Scaling option for variables in X. 
#'    "mean_2sd" scales all continuous and integer-valued variables that are not binary by subtracting the mean and dividing by 2 times the standard deviation. Option "median_2GMD" is a robust alternative subtracting the median and dividing by 2 times the Gini Mean Difference.
#'    "mean_sd_all" and "median_GMD_all" are the counterparts but will scale all variables, including binary/dummy variables. "median_2GMD" and "median_GMD_all" reuqire Hmisc to be installed.
#'    "none" will not scale any variable.
prep_data <- function(d, y_name, y_yes_level = NULL, remove_na = FALSE,
                      ordered_coding = c("dummy", "one-hot", "integer", "none"),
                      unordered_coding = c("dummy", "one-hot"),
                      scale = c("none", "mean_2sd", "mean_sd_all", 
                                "median_2GMD", "median_GMD_all")) {
  # Argument checks
  if (!is.data.frame(d))
    stop("d should be a data frame.")
  if (is.character(y_name) && !all(y_name %in% names(d)))
    stop("y_name should be a string or a vector of strings that match the response variable(s) in d.")
  if (!is.logical(remove_na))
    stop("remove_na should be logical (TRUE / FALSE).")
  
  # Get position(s) of response variable(s)
  # y_name not character is only a hidden functionality without further checks
  idx_y <- if (is.character(y_name)) match(y_name, names(d)) else y_name
  
  # Convert character variables to factor variables
  d[] <- lapply(d, function(x) if (is.character(x)) as.factor(x) else x)
  # Convert logical to to integer variables
  d[] <- lapply(d, function(x) if (is.logical(x)) as.integer(x) else x)
  
  # Define X and y
  y <- d[, idx_y, drop = TRUE]
  X <- d[, -idx_y, drop = FALSE]
  if (remove_na) {
    ok <- complete.cases(X, y) # remove NAs
    X <- X[ok, , drop = FALSE]
    y <- if (length(idx_y) > 1) y[ok, ] else y[ok]
  }
  
  # Handle y
  # y is 1-dimensional response
  if (length(idx_y) == 1 && !is.numeric(y)) {
    if (is.factor(y) && length(unique(y)) == 2) {
      if (!is.null(y_yes_level)) {
        y <- as.integer(y == y_yes_level)
      } else {
        y <- as.integer(y) - 1
        message("y has been converted to integer, where '", levels(y)[1], "' is converted to 0.")
      }
    } else if (is.factor(y)) {
      y <- as.integer(y) - 1
      message("y has been converted to integer, where '", levels(y)[1], "' is converted to 0.")
    }
  }
  # y is multivariate response
  if (length(idx_y) > 1) {
    # Check for special case of 2-dimensional response with 1 numeric
    # and 1 factor with 2 levels component (e.g. time to event)
    # Convert factor component to integer-valued variable
    idx_binary_y <- sapply(y, function(x) is.factor(x) && nlevels(x) == 2)
    if (sum(idx_binary_y) == 1) {
      if (is.null(y_yes_level)) {
        message("factor component of y has been converted to integer, where '", levels(y[, idx_binary_y])[1], "' is converted to 0.")
        y[, idx_binary_y] <- as.integer(y[, idx_binary_y]) - 1
      }
      y[, idx_binary_y] <- as.integer(y[, idx_binary_y] == y_yes_level) 
    }
    # All other factors will just be converted to integer-valued components
    y[] <- lapply(y, function(x) if (is.factor(x)) as.integer(x) - 1 else x) 
  }
  
  # Handle X
  # Ordered factor variables will be coded according to ordered_coding input 
  ordered_coding <- match.arg(ordered_coding)
  f <- function(z) {
    if (is.ordered(z)) {
      switch(ordered_coding,
             "dummy" = factor(z, ordered = FALSE),
             "one-hot" = factor(z, ordered = FALSE),
             "integer" = as.integer(z) - 1,
             z)
    } else {
      z
    }
  }
  X[] <- lapply(X, f)
  
  # The remaining unordered factors will be converted to dummy variables
  unordered_coding <- match.arg(unordered_coding)
  if (any(sapply(X, is.factor))) {
    old_option <- getOption("na.action")
    options(na.action = "na.pass")
    if (unordered_coding == "dummy") {
      X <- model.matrix(~ ., X)[, -1] 
    } else {
      X <- model.matrix(~ 0 + ., X, contrasts.arg = lapply(X[, sapply(X, is.factor), drop = FALSE],
                                                           contrasts, contrasts = FALSE))
    }
    options(na.action = old_option)
  } # X is now a matrix
  
  # Omit constant columns
  idx_const <- constant_columns(X)
  if (length(idx_const) > 0) {
    X <- X[, -idx_const]
    warning(paste0("Constant column ", idx_const, " removed.\n"))
  }
  
  X_unscaled <- X
  scale <- match.arg(scale)
  if (scale == "none")
    return(list(X = X_unscaled, Y = y))
  
  # Depending on scale argument, possibly only scale non-binary ones
  idx_not_binary <- apply(X, 2, function(z) length(unique(z)) != 2)
  
  if (scale == "mean_2sd") {
    # For all except binary variables: subtract mean, divide by 2*SD
    X[, idx_not_binary] <- base::scale(X[, idx_not_binary, drop = FALSE], center = TRUE, 
                                       scale = apply(X[, idx_not_binary, drop = FALSE], 2, function(z) 2 * sd(z, na.rm = TRUE)))
  } 
  if (scale == "mean_sd_all") {
    X <- base::scale(X, center = TRUE, scale = TRUE)
  } 
  if (scale == "median_2GMD") {
    # For all except binary: subtract mean, divide by 2 * Gini's Mean Difference
    stopifnot(requireNamespace("Hmisc", quietly = TRUE))
    X[, idx_not_binary] <- base::scale(X[, idx_not_binary, drop = FALSE], 
                                       center = apply(X[, idx_not_binary, drop = FALSE], 2, median, na.rm = TRUE),
                                       scale =  apply(X[, idx_not_binary, drop = FALSE], 2, function(z) 2 * Hmisc::GiniMd(z, na.rm = TRUE)))
  }
  if (scale == "median_GMD_all") {
    stopifnot(requireNamespace("Hmisc", quietly = TRUE))
    X <- base::scale(X, center = apply(X, 2, median, na.rm = TRUE), 
                     scale = apply(X, 2, function(z) Hmisc::GiniMd(z, na.rm = TRUE)))
  }
  
  list(X = X, X_unscaled = X_unscaled, Y = y)
}
