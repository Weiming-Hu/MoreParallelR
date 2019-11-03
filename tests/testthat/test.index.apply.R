# This script tests the function index.apply
# This is the test file.
context("Function index.apply")

for (progress.bar in c(T, F)) {
  # Define the test data set dimensions
  a1 <- array(1:4000, dim = c(20, 5, 10, 4))
  a2 <- array(1:6000, dim = c(20, 10, 10, 3))
  a3 <- array(1:200, dim = c(20, 1, 10))
  c <- 9.9

  # The sequential solution
  d <- matrix(NA, nrow = 20, ncol = 10)

  for (i in 1:20) {
    for (j in 1:10) {
      v.a1 <- a1[i, , j, ]
      v.a2 <- a2[i, , j, ]
      v.a3 <- a3[i, , j]

      d[i, j] <-
        mean(v.a1) - var(rowMeans(v.a2)) + v.a3 * c
    }
  }

  # The parallel solution
  X <- list(a1, a2, a3)
  MARGIN <- c(1, 3)
  FUN <- function(l, c) {
    # Drop single length dimensions
    l[[3]] <- drop(l[[3]])

    return(mean(l[[2]]) - var(
      rowMeans(l[[3]])) + l[[4]] * c)
  }

  d.par <- index.apply(
    X, MARGIN, FUN, c = c, verbose = T,
    cores = 2, progress.bar = progress.bar)

  # Check
  stopifnot(identical(d, d.par))
}

succeed('Tests passed for the function parallel.apply!')
