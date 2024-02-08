geq <- function(x, y, tol = .Machine$double.eps^0.5) {
  # Test 'greater than or equal to' of two floating point numbers
  # In case x and/or y is a vector, comparison will be done element wise
  x > y | abs(x - y) < tol
}

leq <- function(x, y, tol = .Machine$double.eps^0.5) {
  # Test 'less than or equal to' of two floating point numbers
  # In case x and/or y is a vector, comparison will be done element wise
  x < y | abs(x - y) < tol
}

eq <- function(x, y, tol = .Machine$double.eps^0.5) {
  # Test 'equal to' of two floating point numbers
  # In case x and/or y is a vector, comparison will be done element wise
  abs(x - y) < tol
}
