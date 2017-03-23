## Portfolio class


#' Portfolio Constructor
#'
#' @param x an xts object.
#'
#' @return a porftolio object
#' @export
#'
portfolio <- function(x) {
  #if (!is.xts(x)) stop("X must be an xts object")
  ## !! create as list?
  structure(list(x), class = "portfolio")
}


mean.portfolio <- function(x) {
  
  print("hello")
  
}