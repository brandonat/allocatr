## Optimal portfolio given daily returns

optimal_portofio <- function(x, short_selling = FALSE, max_allocation = NULL) {

  ## k = number of assets
  k <- ncol(x) 
  
  ## covariance matrix and average returns
  cov_mat <- cov(x)
  avg_ret <- colMeans(x)
  
  ## Constraints --------------------------------------------------
  ## lhs, rhs =  left/right hand side
  
  ## weights add to 1
  lhs <- (matrix(1, k)) # Weights add to 1
  rhs <- 1
           
  ## postitive weights?
  if (!short_selling) {
    # positive weights
    lhs <- cbind(1, diag(k)) 
    rhs <- c(rhs, rep(0, k))
  }
  
  ## Minimaze variance --------------------------------------------
  
  res <- solve.QP(Dmat = cov_mat, dvec = avg_ret,
                  Amat = lhs, bvec = rhs, meq = 1)
  
  ## Solution -----------------------------------------------------
  
  ## w = optimal weights
  w <- res$solution
  
  ## ret_p = return of optimal portolio, anualized
  ret_p <- w %*% returns
  ret_p <- (1 + ret_p) ^ 250 - 1
  
  ## var_p = standard deviation, annualized
  var_p <- t(w) %*% cov_mat %*% w
  var_p <- sqrt(var_p * 250)
  
  list(weights = w,
       return = ret_p,
       stdev = var_p)
}

# risk_adusted_ret <- ret_p - tau * var_p
