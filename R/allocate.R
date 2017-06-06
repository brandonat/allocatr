

allocate <- function(p, method = "mv", ...) {

  if (method == "mv") return(allocate_mv(p, ...))

  ## add methods for:
  ##  mv with covariance matrix shrinkage
  ##  resampled mv
  ##  monte carlo
  ##  machine learning??
  ##  others??

}

## !! will this provide the optimal? or Eff. Frontier as well?
allocate_mv <- function(p, short_selling = FALSE, max_allocation = NULL) {

    ## For now, p = returns. Will change to portfolio object's returns
    ## x <- p$returns
    x <- p

    ## k assets
    k <- ncol(x)

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
