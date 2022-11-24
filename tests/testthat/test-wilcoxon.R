test_that("multiplication works", {
  set.seed(456)
  x <- matrix(rnorm(100), 10, 10)
  for(i in 1:10)
    expect_equal(wilcox.test(x[, i])$p.value, unname(wilcoxon.test(x[, i])$pvalue))

  # Paired two-sample test
  y <- matrix(rnorm(100), 10, 10)
  for(i in 1:10){
    expect_equal(wilcox.test(x[, i], y[, i], paired=TRUE)$p.value,
                 unname(wilcoxon.test(x[, i], y[, i], paired=TRUE)$pvalue))
    expect_equal(wilcox.test(x[, i] - y[, i])$p.value,
                 unname(wilcoxon.test(x[, i] - y[, i])$pvalue))
  }

  # Two-sample test
  for(i in 1:10){
    expect_equal(wilcox.test(x[, i], y[1:5, i])$p.value,
                 unname(wilcoxon.test(x[, i], y[1:5, i])$pvalue))
  }
})
