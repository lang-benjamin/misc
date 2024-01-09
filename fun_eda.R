# Provide summary statistics for cleaning EDA
# Function is taken from John Curtin: https://github.com/jjcurtin/lab_support/blob/main/fun_eda.R
skim_some <- skimr::skim_with(numeric = sfl(mean = NULL, sd = NULL, p25 = NULL, p50 = NULL, p75 = NULL, hist = NULL))

# Function is taken from John Curtin: https://github.com/jjcurtin/lab_support/blob/main/fun_eda.R
print_kbl <- function(data, height = "500px", align = "r", digits = 2, caption = NULL) {
  data |>
    kableExtra::kbl(align = align, digits = digits, caption = caption) |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "condensed")) |>
    kableExtra::scroll_box(height = height, width = "100%")
}

# Function is taken from John Curtin: https://github.com/jjcurtin/lab_support/blob/main/fun_eda.R
# Somewhat unformatted printing of text responses for categorical variables.
# Used primarily to confirm that responses are valid and tidy
print_responses <- function(name, column){
  unique(column) |>
    na.omit() |>
    stringr::str_c(collapse = ", ") |>
    stringr::str_c(name, ": ", ., "\n") |>
    cat()
}
