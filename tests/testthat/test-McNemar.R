test_that("multiplication works", {
  set.seed(456)
  x <- matrix(sample(10, 160, replace=TRUE), 4, 40) + 10
  for(i in 1:40){
    t <- matrix(x[, i], 2, 2)
    expect_equal(mcnemar.test(t)$p.value,
                 unname(McNemar.test(t)$pvalue))
    }
})
