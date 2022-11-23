#'wilcoxon.test
#'
#'\code{wilcoxon.test} performs one and two-sample Wilcoxon tests on vectors of data.
#'
#'@usage wilcoxon.test <- function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
#'                       mu = 0, paired = FALSE, exact = NULL, correct = TRUE)
#'
#'@param x a numeric vector or matrix.
#'@param y an optional numeric vector, of same length of \code{x} if \code{paired=TRUE}.
#'@param alternative a string specifying the alternative hypothesis.
#'@param mu an optional parameter of the null hypothesis with default \code{mu=0}.
#'@param paired a logical indicating this is a paired test.
#'@param exact a logical indicating whether using a exact p-value.
#'@param correct a logical indicating whether using continuity correction.
#'
#'@returns \item{method}{a string describing the method used.}
#'@returns \item{data.name}{a string give the name of the data used.}
#'@returns \item{stat}{the value of the chi-squared test statistic.}
#'@returns \item{pvalue}{the p-value of the test statistic.}
#'@returns \item{alter}{the string of alternative hypothesis.}
#'
#'@examples
#'x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
#'y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
#'
#'# Paired two-sample Wilcoxon test.
#'wilcoxon.test(x, y, paired = TRUE)
#'
#'# One-sample Wilcoxon test (equivalent to the above test)
#'wilcoxon.test(x - y, alternative = "less")
#'
#'# One-sample Wilcoxon test with a position parameter.
#'wilcoxon.test(c(1:100, 1:100), mu=50)
#'
#'# Two-sample Wilcoxon test.
#'wilcoxon.test(x, y)
#'
#'@export
#'
wilcoxon.test <- function(x, y = NULL, alternative = "two.sided",
                          mu = 0, paired = FALSE, exact = NULL, correct = TRUE){
  data.name <- deparse(substitute(x))

  if(paired){
    # Paired-sample test is equivalent to One-sample test
    data.name <- paste(data.name, 'and', deparse(substitute(y)))
    x <- x - y

    alter <- switch(alternative,
                    "two.sided" =
                      paste("alternative hypothesis: true location shift is not euqal to", mu),
                    "less" =
                      paste("alternative hypothesis: true location shift is less than", mu),
                    "greater" =
                      paste("alternative hypothesis: true location shift is greater than", mu))
  }else
    alter <- switch(alternative,
                    "two.sided" =
                      paste("alternative hypothesis: true location is not euqal to", mu),
                    "less" =
                      paste("alternative hypothesis: true location is less than", mu),
                    "greater" =
                      paste("alternative hypothesis: true location is greater than", mu))

  if(is.null(y) | paired){
    # One-sample test
    x <- x - mu
    x <- x[x != 0]
    n <- length(x)
    rk <- rank(abs(x))
    T <- sum(rk[sign(x) == 1])

    if(is.null(exact)) exact <- n < 50

    tied <- length(rk) != length(unique(rk))
    if(exact & !tied){
      pvalue <- switch(alternative,
                       "two.sided" = {
                         p <- if(T > (n * (n + 1) / 4))
                           psignrank(T - 1, n, lower.tail = FALSE)
                         else psignrank(T, n)
                         min(2 * p, 1)
                       },
                       "less" = psignrank(T, n),
                       "greater" = psignrank(T - 1, n, lower.tail = FALSE))
      method <- "\tWilcoxon signed rank exact test"
    } else {
      method <- "\tWilcoxon signed rank test"
      Z <- T - n * (n + 1) / 4

      # Continuity correction
      if(correct){
        correction <- switch(alternative,
                             "two.sided" = sign(Z) * 0.5,
                             'less' = -0.5,
                             'greater' = 0.5)
        method <- paste(method, "with continuity correction")
      }

      # Tied correction
      tied_correction <- sum(table(rk) ** 3 - table(rk)) / 48

      Z <- (Z - correction) / sqrt(n * (n + 1) * (2 * n + 1) / 24 - tied_correction)
      pvalue <- switch(alternative,
                       "two.sided" = 2 * min(pnorm(Z), pnorm(Z, lower.tail = FALSE)),
                       "less" = pnorm(Z),
                       "greater" = pnorm(Z, lower.tail = FALSE))
      if(exact & tied)
        warning("cannot compute exact p-value with ties")
    }
    names(T) <- 'V'
  } else {
    # Two-sample test, aka Mann-Whitney-U Test
    m <- length(x)
    n <- length(y)
    N <- m + n

    val <- c(x - mu, y)
    group <- c(rep(1, m), rep(2, n))

    ord <- order(val)
    rk <- rank(val)[ord]
    group <- group[ord]

    T <- sum(rk[group == 1]) - m * (m + 1) / 2

    tied <- length(rk) != length(unique(rk))
    if(is.null(exact)) exact <- (m < 50) & (n < 50)
    if(exact & !tied){
      method <- "\tWilcoxon signed rank exact test"
      pvalue <- switch(alternative,
                       "two.sided" = {
                         p <- if(T > (m * n / 2)) pwilcox(T - 1, m, n, lower.tail = FALSE)
                         else pwilcox(T, m, n)
                         min(2 * p, 1)
                       },
                       "greater" = pwilcox(T - 1, m, n, lower.tail = FALSE),
                       "less" = pwilcox(T, m, n))
    } else {
      method <- "\tWilcoxon signed rank test"
      Z <- T - m*n/2

      # Continuity correction
      if(correct){
        correction <- switch(alternative,
                             "two.sided" = sign(Z) * 0.5,
                             'less' = -0.5,
                             'greater' = 0.5)
        method <- paste(method, "with continuity correction")
      }

      # Tied correction
      tied_correction <- m * n * sum(table(rk) ** 3 - table(rk)) / 12 / N / (N - 1)

      Z <- (Z - correction) / sqrt(m*n*(N+1)/12 - tied_correction)
      pvalue <- switch(alternative,
                       "two.sided" = 2 * min(pnorm(Z), pnorm(Z, lower.tail = FALSE)),
                       "less" = pnorm(Z),
                       "greater" = pnorm(Z, lower.tail = FALSE))
      if(exact & tied)
        warning("cannot compute exact p-value with ties")
    }
    names(T) <- 'W'
  }
  names(pvalue) <- 'p-value'

  res <- structure(list(method = method,
                        data.name = data.name,
                        stat = T,
                        pvalue = pvalue,
                        alter = alter),
                   class = 'nonparam')
  return(res)
}


