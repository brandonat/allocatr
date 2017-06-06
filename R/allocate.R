

allocate <- function(p, method = "mv", ...) {

  returns <- p$returns[-1] # remove date

  if (method == "mv") return(allocate_mv(returns, ...))
  if (method == "resampled_mv") return(allocate_rmv(returns, ...))

  ## add methods for:
  ##  mv with covariance matrix shrinkage
  ##  resampled mv
  ##  monte carlo
  ##  machine learning??
  ##  others??

}


#' Allocate with Mean Variance Optimization
#'
#' @param p a portofio object.
#' @param short_selling whether short selling is allowed.
#' @param max_allocation optional max percent for any single asset.
#'
#' @return a list of optimal weights, expected return, and expected volatility.
#' @export
#'
#' @examples
allocate_mv <- function(returns, short_selling = FALSE, max_allocation = NULL) {

    ## Returns and number of assets
    x <- returns
    k <- ncol(returns)

    ## covariance matrix and average returns
    cov_mat <- cov(x)
    avg_ret <- colMeans(x)

    ## Constraints
    ## --------------------------------------------------

    ## Weights add to 1
    lhs <- (matrix(1, k)) # weights add to 1
    rhs <- 1

    ## Postitive weights?
    if (!short_selling) {
      lhs <- cbind(lhs, diag(k))
      rhs <- c(rhs, rep(0, k))
    }

    ## Max allocation to single asset
    if(!is.null(max_allocation)){
      if(max_allocation > 1 | max_allocation <0){
        stop("max_allocation must be greater than 0 and less than 1")
      }
      if(max_allocation * k < 1){
        stop("Need to set max_allocation higher; not enough assets to add to 1")
      }
      lhs <- cbind(lhs, -diag(k))
      rhs <- c(rhs, rep(-max_allocation, k))
    }

    ## Solve
    ## --------------------------------------------------
    res <- quadprog::solve.QP(
      Dmat = cov_mat, dvec = avg_ret,
      Amat = lhs, bvec = rhs, meq = 1)

    ## w = optimal weights
    w <- res$solution

    ## Unannualized return and stdev
    ret_p <- w %*% avg_ret
    var_p <- sqrt(t(w) %*% cov_mat %*% w)

    list(weights = round(w, 5),
         return = ret_p,
         stdev = var_p)
}

allocate_rmv <- function(returns, iter = 100, ...) {

  x <- returns
  k <- ncol(x)
  m <- nrow(x)

  ## Bagging weights
  bag_w <- as.data.frame(
    matrix(0, nrow = iter, ncol = k))
  names(bag_w) <- c(names(x))

  for (i in 1:iter) {
    index <- sample(m, replace = TRUE)
    res <- allocate_mv(x[index, ], ...)

    bag_w[i, ] <- res$weights
  }

  ## Aggregate using column means
  w <- colMeans(bag_w)

  ## todo: calc ret_p and std_p

  w
}
