## Portfolio class and associated functions

#' Portfolio Constructor
#'
#' @param x an xts object.
#'
#' @return a porftolio object
#' @export
#'
portfolio <- function(symbols = NULL, prices = NULL, weights = NULL, ...) {

  ## Object is built using either 'symbols' or 'prices' only
  ## First check if 'prices' was provided to avoid extra work
  if (!is.null(prices)) {

    if (!tibble::is_tibble(prices)) stop("prices must be a tbl_df object. use 'get_prices()'")
    symbols <- names(prices)[-1] # remove date

  } else if (!is.null(symbols)) {

    prices <- get_prices(symbols, ...) # this can be slow

  } else {

    stop("symbols or prices must be provided")
  }

  ## Other calculated or assumed components

  ## n = number of assets
  n <- length(symbols)

  ## If weights not provided, assume equal across assets
  ## The allocation functions will reset this with an "optimal allocation"
  if (is.null(weights)) {
    weights <- rep(1 / n, n)
  }

  ## Daily returns
  returns <- daily_returns(prices)

  ## Historical weights based on weights at beginning prices.
  h_weights <- h_weights(weights, returns)

  ## Rebalancing
  rebalance <- NULL

  ## Return as list of class 'portfolio'
  structure(
    list(
      symbols = symbols,
      prices = prices,
      n = n,
      weights = weights,
      returns = returns,
      h_weights = h_weights,
      rebalance = rebalance
    ),
    class = "portfolio")
}

## Functions to describe portolio

mean.portfolio <- function(x) {

  print("hello")

}

## Functions to assess portfolio performance
