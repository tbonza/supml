#' Testing data documentation

#' mtcars dataset
#'
#' Data from the mtcars dataset included in R
#' by default. 
#' 
#' @docType data
#' 
#' @usage data(mtcars)
"mtcars"

#' mtcars derivative variables
#' 
#' Data derived from mtcars for testing 
#' Gaussian kernel. Tests fail if I don't
#' overexplain how data is generated for
#' tests.
#'
#' Generated: 
#'
#' y = as.matrix(mtcars$mpg)
#' x = as.matrix(mtcars$wt)
#' w = runif(length(x))
#'
#' @docType data
#' 
#' @usage data(x,w,y)
"w"

#' mtcars derivative variables
#' 
#' Data derived from mtcars for testing 
#' Gaussian kernel. Tests fail if I don't
#' overexplain how data is generated for
#' tests.
#'
#' Generated: 
#'
#' y = as.matrix(mtcars$mpg)
#' x = as.matrix(mtcars$wt)
#' w = runif(length(x))
#'
#' @docType data
#' 
#' @usage data(x,w,y)
"x"

#' mtcars derivative variables
#' 
#' Data derived from mtcars for testing 
#' Gaussian kernel. Tests fail if I don't
#' overexplain how data is generated for
#' tests.
#'
#' Generated: 
#'
#' y = as.matrix(mtcars$mpg)
#' x = as.matrix(mtcars$wt)
#' w = runif(length(x))
#'
#' @docType data
#' 
#' @usage data(x,w,y)
"y"
