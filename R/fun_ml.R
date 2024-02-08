# Function taken from John Curtin: https://github.com/jjcurtin/lab_support/blob/main/fun_ml.R
# prep a recipe and bake some data
# By default, this function will glimpse your new features
# Set glimpse_it to FALSE to suppress this
make_features <- function(rec, data_trn, data_new = NULL, glimpse_it = TRUE){

  features <- rec %>%
    recipes::prep(training = data_trn, strings_as_factors = FALSE) %>%
    recipes::bake(new_data = data_new)

  if (glimpse_it){
    features %>% dplyr::glimpse()
  }

  return(features)
}

# Define custom function for generating permutation-based p-values (similar to Altman et al)
# using the R package MachineShop, see https://github.com/brian-j-smith/MachineShop/issues/10
# Argument x is the difference between permuted and observed model performances for a variable
pval_perm <- function(x) {
  c("pvalue" = min(2 * mean(x <= 0), 1))
}
# Use this via MachineShop::varimp()
#permpval <- varimp(
#  model_fit,
#  scale = FALSE,
#  samples = 1000,
#  stats = pval_perm
#)
#plot(permpval) + labs(y = "Permutation p-value")

# TODO
# - function that creates variable importance via effects style plot (effect of moving from Q1 to Q3)