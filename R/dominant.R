#' Return the dominant class (the mode of a classified variable)
#'
#' This is a helper method used to return the dominant class of a classified variable.
#' This is equivalent to median for a numeric variable.
#'
#' @param x character array, can also be a factor
#' @return a 1-length character of the dominant class
#' @export
#' @examples
#' dominant(sample(letters, 100, replace=T))
dominant <- function(x) names(which.max(table(x, useNA="no")))[1]
