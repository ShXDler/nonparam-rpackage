#'print.nonparam
#'
#'\code{print.nonparam} prints objects of class "\code{nonparam}" used in this package.
#'
#'@usage print.nonparam(x)
#'
#'@param x an object of class "\code{nonparam}"
#'
#'@returns the argument \code{x}, invisibly
#'
#'@examples
#'t <- chi.square.test(1:6)
#'print(t)
#'
#'@export
#'
print.nonparam <- function(x){
  cat(x$method, '\n\n')
  cat('data: ', x$data.name, '\n')
  out <- character(0)
  if(!is.null(x$stat))
    out <- c(paste(names(x$stat), "=", format(signif(x$stat, 5))))
  if(!is.null(x$para))
    out <- c(out, paste(names(x$para), "=", format(signif(x$para, 5))))
  out <- c(out, paste("p-value =", format.pval(x$pvalue, digits=4)))
  cat(out, sep=', ')
  cat('\n')
  if(!is.null(x$alter))
    cat(x$alter, '\n')
  cat('\n')
}
