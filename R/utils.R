## Utilities

## Convert from daily to yearly returns
return_dy <- function(x) {
  (1 + x) ^ 250 - 1
}

## Convert from yearly to daily returns
return_yd <- function(x) {
  (1 + x) ^ (1 / 250) - 1
}

xts2tbl <- function(x) {

  x1 <- data.frame(date = zoo::index(x))
  x2 <- as.data.frame(x, row.names = NULL)
  tibble::as_tibble(cbind(x1, x2))
}

tbl2xts <- function(x) {

  ## Date stored in first column
  xts::xts(x[, -1], order.by = x$date)
}
