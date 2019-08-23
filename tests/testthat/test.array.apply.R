# This script tests the function array.apply
# This is the test file.
context("Function array.apply")

# Define the test data set dimensions
dims <- c(10, 20, 5, 8)
X <- array(data = runif(prod(dims)), dim = dims)

# Randomly assign special values
count <- length(X) * 0.01
X[sample.int(length(X), size = count)] <- NA
X[sample.int(length(X), size = count)] <- -Inf
X[sample.int(length(X), size = count)] <- Inf

# Define some other test variables
FUN <- mean
cores <- 2

for (m in 1:4) {
  combns <- combn(x = 1:length(dims), m = m)

  for (i.col in 1:ncol(combns)) {

    MARGIN <- combns[, i.col]

    # Carry out the test
    ret <- array.apply(
      X, MARGIN, cores, FUN, na.rm = T)
    ret.serial <- apply(
      X, MARGIN, FUN, na.rm = T)

    # Check results
    if (length(MARGIN) == 1) {
      stopifnot(identical(as.vector(ret.serial), as.vector(ret)))
    } else {
      stopifnot(identical(ret.serial, ret))
    }
  }
}

succeed('Tests passed for the function array.apply!')
