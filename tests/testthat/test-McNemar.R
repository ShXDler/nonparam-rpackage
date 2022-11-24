test_that("multiplication works", {
  set.seed(456)
  x <- matrix(sample(10, 80, replace=TRUE), 4, 20) + 10
  for(i in 1:20){
    t <- matrix(x[, i], 2, 2)
    expect_equal(mcnemar.test(t)$p.value,
                 unname(McNemar.test(t)$pvalue))
    expect_equal(mcnemar.test(t, correct=FALSE)$p.value,
                 unname(McNemar.test(x[, i], correct=FALSE)$pvalue))
    }
})
