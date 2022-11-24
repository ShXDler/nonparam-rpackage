test_that("fisher.exact.test works", {
  set.seed(456)
  x <- matrix(sample(10, 80, replace=TRUE), 4, 20) + 10
  for(i in 1:20){
    t <- matrix(x[, i], 2, 2)
    expect_equal(fisher.test(t, alternative = 'less')$p.value,
                 unname(fisher.exact.test(t, alternative = 'less')$pvalue))
  }
  for(i in 1:20){
    t <- matrix(x[, i], 2, 2)
    expect_equal(fisher.test(t, alternative = 'greater')$p.value,
                 unname(fisher.exact.test(t, alternative = 'greater')$pvalue))
  }
  for(i in 1:20){
    t <- matrix(x[, i], 2, 2)
    expect_equal(fisher.test(t)$p.value,
                 unname(fisher.exact.test(t)$pvalue))
  }
})
