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

  ## Get Adjsuted Close* price from environment with price data
  ## *takes into account splits and dividends
  close_prices <- function(sym, envir) {
    out <- get(sym, envir = envir)
    out <- quantmod::Ad(out)
    names(out) <- sym
    out
  }
  x <- do.call(
    xts::cbind.xts,
    lapply(symbols, close_prices, envir = prices_env)
  )
  na.omit(x)
}

daily_returns <- function(x) {

  out <- vapply(x, quantmod::dailyReturn, numeric(nrow(x)))
  xts::xts(out, order.by = zoo::index(x))
}

monthly_returns <- function(x) {

  out <- vapply(x, quantmod::monthlyReturn, numeric(nrow(x)))
  xts::xts(out, order.by = zoo::index(x))
}



