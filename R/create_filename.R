#' Create file name with time stamp
#' 
#' @param main The main file name to be used
#' @param extension The file extension to be used, e.g. "csv"
#' @examples
#' create_filename("foo","csv")
create_filename <- function(name, extension, time = FALSE) {
  if (time) {
    paste0(name, format(Sys.time(),'_%Y-%m-%d_%H%M'), ".", extension)
  } else {
    paste0(name, format(Sys.time(),'_%Y-%m-%d'), ".", extension)
  }
}
