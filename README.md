# Nonparam

  [![R-CMD-check](https://github.com/ShXDler/nonparam-rpackage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ShXDler/nonparam-rpackage/actions/workflows/R-CMD-check.yaml)

## Overview

Nonparam is an R package for several common non-parametric test methods. Currently, the functions supported are listed as follows.

-   `chi.square.test()` performs chi-square goodness-of-fit tests and contingency table tests on a matrix.
-   `wilcoxon.test()` performs one and two-sample Wilcoxon tests on vectors of data.
-   `McNemar.test()` performs McNemar's chi-squared test for 2\$\times\$2 table.
-   `fisher.exact.test()` performs Fisher's exact test on 2\$\times\$2 contingency table.

These functions have similar usage and are easy to apply on different data structures. The returned values of these function are objects of class `nonparam`. We define a print function `print.nonparam()` to display the returned object.

## Installation

```{r}
# The easiest way to download Nonparam
install.packages("Nonparam")

# Build Vignettes
devtools::install(build_vignettes = T)
```

## Usage

```{r}
library(Nonparam)

# Chi-squared goodness-of-fit tests on a vector
x <- c(12, 5, 7, 9, 8, 1)
chi.square.test(x)

# Paired two-sample Wilcoxon test.
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06)
wilcoxon.test(x, y, paired = TRUE)

# McNemar test
x <- matrix(c(794, 86, 150, 570), 2, 2)
McNemar.test(x)

# Fisher Exact Test
x <- matrix(c(2, 10, 15, 3), 2, 2)
fisher.exact.test(x)
```

## Getting help

If you have encountered a bug or unexpected result, please raise an issue with a reproducible example.
