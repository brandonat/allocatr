## Get price data

#' Get securities prices
#'
#' @param symbols a character vector of securities tickers.
#' @param ... additional arguments to pass to quantmod's \code{getSymbols()}.
#'
#' @return an xts object with a column for each symbol.
#'
#' @export
#'
#' @examples
#' get_prices(c("AAPL", "IBM"), from = "2010-01-01")
get_prices <- function(symbols, ...) {

  ## Default start date is 2007-01-01
  ## To change that, add an argument: from = "YYYY-MM-DD"

  ## Download OHLC data
  prices_env <- new.env()
  suppressWarnings(
    quantmod::getSymbols(symbols, env = prices_env,  ...))

  ## Get "Adjsuted Close" price from environment with price data
  ## to take into account splits and dividends
  close_prices <- function(sym, envir) {
    out <- get(sym, envir = envir)
    out <- quantmod::Ad(out)
    names(out) <- sym
    out
  }

  ## Combine into an xts object with one column per symbol
  x <- do.call(
    xts::cbind.xts,
    lapply(symbols, close_prices, envir = prices_env)
  )

  ## Return as tbl_df
  xts2tbl(na.omit(x))
}

daily_returns <- function(prices) {

  x <- tbl2xts(prices)
  out <- vapply(x, quantmod::dailyReturn, numeric(nrow(x)))
  out <- xts::xts(out, order.by = zoo::index(x))
  xts2tbl(out)
}

## !! apply function not working
# monthly_returns <- function(prices) {
#
#   x <- tbl2xts(prices)
#   out <- sapply(x, quantmod::monthlyReturn)
#   out <- xts::xts(out, order.by = zoo::index(x))
#   tbl2xts(out)
# }

cumulative_returns <- function(returns) {

  date <- returns[, 1]
  x <- 1 + returns[, -1]
  out <- vapply(x, cumprod, numeric(nrow(x)))
  out <- as.data.frame(out)
  tibble::as_tibble(cbind(date, out))
}

h_weights <- function(weights, returns) {

  date <- returns[, 1]
  n <- ncol(returns) - 1

  ## Set initial values to weights then apply daily returns thereafter
  tmp <- 1 + returns[, -1]
  tmp[1, ] <- weights
  tmp <- vapply(tmp, cumprod, numeric(nrow(tmp)))
  tmp <- data.frame(tmp)

  ## Calculate sum across rows
  row_sum <- rowSums(tmp)

  ## Then recalculate weights for a historical series
  out <- tmp
  for (i in 1:n) {
    out[, i] <- out[, i] / row_sum
  }

  tibble::as_tibble(cbind(date, out))
}

