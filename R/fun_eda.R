print_levels <- function(d, print_continuous = FALSE, sort_variables = FALSE) {
  variable_names <- names(d)  # Get all variable names
  
  if (sort_variables) {
    variable_names <- sort(variable_names)
  }
  
  for (i in seq_along(variable_names)) {
    x <- variable_names[i]
    
    # Skip continuous variables entirely if print_continuous = FALSE
    if ((is.numeric(d[[x]]) || is.integer(d[[x]])) && !print_continuous) {
      next  # Skip to the next variable
    }
    var_label <- Hmisc::label(d[[x]])
    if (var_label == "")
      cat(x, ": ", sep = "")
    else
      cat(x, " (", var_label, "): ", sep = "")
    
    if (is.factor(d[[x]])) {
      cat(paste(levels(d[[x]]), collapse = "; "), if (is.ordered(d[[x]])) "(ordered factor)" else "(unordered factor)", "\n")
    } else if (is.character(d[[x]])) {
      cat(paste(unique(d[[x]]), collapse = "; "), "(unique values)", "\n")
    } else if (is.numeric(d[[x]]) || is.integer(d[[x]])) {
      if (print_continuous) {
        lowest_values <- sort(d[[x]])[1:5]
        highest_values <- sort(d[[x]], decreasing = TRUE)[1:5]
        cat("lowest: ", paste(lowest_values, collapse = " "),
            ", highest: ", paste(highest_values, collapse = " "), "\n", sep = "")
      }
    }
    if (i < length(variable_names)) {
      cat("\n")
    }
  }
}