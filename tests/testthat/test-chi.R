test_that("chi.square.test works", {
  set.seed(456)
  x <- matrix(sample(10, 150, replace=TRUE), 10, 15) + 10
  for(i in 1:15)
    expect_equal(chisq.test(x[, i])$p.value,
                 unname(chi.square.test(x[, i])$pvalue))

  # Pearson Chi-squared test
  y <- sample(10, 100, replace=TRUE) + 10
  for(i in 0:24){
    z <- matrix(y[(4 * i + 1): (4 * i + 4)], 2, 2)
    expect_equal(chisq.test(z, correct=FALSE)$p.value,
                 unname(chi.square.test(z)$pvalue))
  }
})
