% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prices.R
\name{get_prices}
\alias{get_prices}
\title{Get securities prices}
\usage{
get_prices(symbols, ...)
}
\arguments{
\item{symbols}{a character vector of securities tickers.}

\item{...}{additional arguments to pass to quantmod's \code{getSymbols()}.}
}
\value{
an xts object with a column for each symbol.
}
\description{
Get securities prices
}
\examples{
get_prices(c("AAPL", "IBM"), from = "2010-01-01")
}
