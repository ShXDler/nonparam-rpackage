#'fisher.exact.test
#'
#'\code{fisher.exact.test} performs Fisher's exact test on 2\eqn{\times}2 contingency table.
#'
#'@usage fisher.exact.test <- function(x, alternative = c("two.sided", "less", "greater"))
#'
#'@param x a numeric vector or matrix.
#'@param alternative a string specifying the alternative hypothesis.
#'
#'@returns \item{method}{a string describing the method used.}
#'@returns \item{data.name}{a string give the name of the data used.}
#'@returns \item{pvalue}{the p-value of the test statistic.}
#'@returns \item{alter}{the string of alternative hypothesis.}
#'
#'@examples
#'# Fisher Exact Test
#'
#'x <- matrix(c(2, 10, 15, 3), 2, 2)
#'fisher.exact.test(x)
#'fisher.exact.test(x, alternative = "greater")
#'
#'@export
#'
fisher.exact.test <- function(x, alternative = 'two.sided'){
  data.name <- deparse(substitute(x))
  method <- "\tFisher's Exact Test for Count Data"

  alter <- switch(alternative,
                  "two.sided" =
                    paste("alternative hypothesis: true odds ratio is not euqal to 1"),
                  "less" =
                    paste("alternative hypothesis: true odds ratio is less than 1"),
                  "greater" =
                    paste("alternative hypothesis: true odds ratio is greater than 1"))

  A <- x[1, 1]
  m <- A + x[1, 2]
  n <- x[2, 1] + x[2, 2]
  k <- A + x[2, 1]

  twosidehyper <- function(A, m, n, k){
    plist <- dhyper(max(0, k-m): min(k, m), m, n, k)
    p <- dhyper(A, m, n, k)
    if(p == max(plist)) return(1)
    return(sum(plist[plist <= p]))
  }
  pvalue <- switch(alternative,
                   'two.sided' = twosidehyper(A, m, n, k),
                   'less' = phyper(A, m, n, k),
                   'greater' = phyper(A-1, m, n, k,lower.tail = FALSE)
                   )

  res <- structure(list(method = method,
                        data.name = data.name,
                        pvalue = pvalue,
                        alter = alter),
                   class = 'nonparam')
  return(res)
}
