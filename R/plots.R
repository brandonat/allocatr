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

  x <- cumulative_returns(rets)
  x <- tidyr::gather(x, "tkr", "return", 2:ncol(x))

  ggplot(x) +
    geom_line(aes(x = date, y = return, color = tkr)) +
    labs(x = "Date", y = "Cumulative Returns", color = "") +
    theme_minimal()
}

#' Plot weights
#'
#' @param h_weights xts object of historical portofio weights.
#'
#' @return
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#' @export
#'
plot_weights <- function(h_weights, limits = c(0, 1)) {

  x <- h_weights %>%
    tidyr::gather("tkr", "weight", 2:ncol(h_weights))

  ggplot(x) +
    geom_line(aes(x = date, y = weight, color = tkr)) +
    labs(x = "Date", y = "Portoflio Weights", color = "") +
    scale_y_continuous(limits = limits) +
    theme_minimal()

}
