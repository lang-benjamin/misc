# Provide summary statistics for cleaning EDA
# Function is taken from John Curtin: https://github.com/jjcurtin/lab_support/blob/main/fun_eda.R
skim_some <- skimr::skim_with(numeric = skimr::sfl(mean = NULL, sd = NULL, p25 = NULL, p50 = NULL, p75 = NULL, hist = NULL))

# Function is taken from John Curtin: https://github.com/jjcurtin/lab_support/blob/main/fun_eda.R
print_kbl <- function(data, height = "500px", align = "r", digits = 2, caption = NULL) {
  data |>
    kableExtra::kbl(align = align, digits = digits, caption = caption) |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "condensed")) |>
    kableExtra::scroll_box(height = height, width = "100%")
}

# Print levels or unique values of factor / character variables in a data frame
print_levels <- function(d) {
  for (x in names(d)) {
    if (is.factor(d[[x]])) {
      cat("Variable:", x, "\n")
      cat("Levels:", paste(levels(d[[x]]), collapse = "; "), "\n\n")
    } else if (is.character(d[[x]])) {
      cat("Variable:", x, "\n")
      cat("Unique Values:", paste(unique(d[[x]]), collapse = "; "), "\n\n")
    }
  }
}