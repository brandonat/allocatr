## Plot returns

#' Plot returns
#'
#' @param rets periodic returns.
#'
#' @return
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' x <- get_prices(c("AAPL", "IBM"), from = "2010-01-01")
#' rets <- daily_returns(x)
#' plot_returns(rets)
#'
plot_returns <- function(rets) {

  ## Plot cumulative returns of an xts object with multiple stock returns

  x <- rets + 1
  x <- vapply(x, cumprod, numeric(nrow(x)))
  x <- data.frame(date = zoo::index(rets), x)
  x <- tidyr::gather(x, "tkr", "return", 2:ncol(x))

  ggplot(x) +
    geom_line(aes(x = date, y = return, color = tkr)) +
    labs(x = "Date", y = "Cumulative Returns", color = "") +
    theme_minimal()
}
