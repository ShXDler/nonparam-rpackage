#'McNemar.test
#'
#'\code{McNemar.test} performs McNemar's chi-squared test for 2\eqn{\times}2 table.
#'
#'@usage McNemar.test <- function(x, correct = TRUE)
#'
#'@param x a numeric vector or matrix.
#'@param correct a logical indicating whether using continuity correction.
#'
#'@returns \item{method}{a string describing the method used.}
#'@returns \item{data.name}{a string give the name of the data used.}
#'@returns \item{stat}{the value of the chi-square test statistic.}
#'@returns \item{para}{the degree of freedom of the test statistic.}
#'@returns \item{pvalue}{the p-value of the test statistic.}
#'
#'@examples
#'
#'# McNemar test
#'x <- matrix(c(794, 86, 150, 570), 2, 2)
#'McNemar.test(x)
#'
#'@export
#'
McNemar.test <- function(x, correct = TRUE) {
  data.name <- deparse(substitute(x))
  B <- x[1, 2]
  C <- x[2, 1]
  method <- "\tMcNemar's Chi-squared test"
  if (correct & (B != C)) {
    correction <- 1
    method <- paste(method, "with continuity correction")
  }
  chisq <- (abs(B - C) - correction) ^ 2 / (B + c)
  df <- 1
  pvalue <- 1 - pchisq(chisq, df)

  names(df) <- 'df'
  names(chisq) <- "McNemar's chi-squared"
  names(pvalue) <- 'p-value'

  res <- structure(list(method = method,
                        data.name = data.name,
                        stat = chisq,
                        para = df,
                        pvalue = pvalue),
                   class = 'nonparam')
  return(res)
}
