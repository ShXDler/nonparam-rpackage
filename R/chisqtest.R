#'chi.square.test
#'
#'\code{chi.square.test} performs chi-squared goodness-of-fit tests on a vector and contingency table tests on a matrix.
#'
#'@usage chi.square.test(x, p = rep(1 / length(x), length(x)))
#'
#'@param x a numeric vector or matrix.
#'@param p a vector of probabilities of the same length as x, only used when x is a vector.
#'
#'@returns \item{method}{a string describing the method used.}
#'@returns \item{data.name}{a string give the name of the data used.}
#'@returns \item{stat}{the value of the chi-squared test statistic.}
#'@returns \item{para}{the degree of freedom of the test statistic.}
#'@returns \item{pvalue}{the p-value of the test statistic.}
#'
#'@examples
#'# Chi-squared goodness-of-fit tests on a vector
#'x <- c(12, 5, 7, 9, 8, 1)
#'chi.square.test(x)
#'
#'# Chi-squared contigency table tests on a matrix
#'x <- as.matrix(c(12, 5, 7, 9, 8, 1), 2, 3)
#'chi.square.test(x)
#'
#'@export
#'
chi.square.test <- function(x, p = rep(1 / length(x), length(x))){
  data.name <- deparse(substitute(x))
  x <- drop(as.matrix(x))

  if(is.matrix(x)){
    # Pearson-Chi Square test for multiple samples
    n <- sum(x)
    col.p <- colSums(x) / n
    row.p <- rowSums(x) / n
    E <- outer(row.p, col.p) * n
    chisq <- sum((x - E) ** 2 / E)
    df <- (ncol(x) - 1) * (nrow(x) - 1)
    pvalue = 1 - pchisq(chisq, df)
    method <- "\tPearson's Chi-squared test"

  } else {
    # One-sample test
    n <- sum(x)
    E <- n * p
    chisq <- sum((x - E) ** 2 / E)
    df <- length(x) - 1
    pvalue = 1 - pchisq(chisq, df)
    method <- "\tChi-squared test for given probabilities"
  }

    names(df) <- 'df'
    names(chisq) <- 'X-squared'
    names(pvalue) <- 'p-value'

    res <- structure(list(method = method,
                          data.name = data.name,
                          stat = chisq,
                          para = df,
                          pvalue = pvalue),
                     class = 'nonparam')
    return(res)
}
