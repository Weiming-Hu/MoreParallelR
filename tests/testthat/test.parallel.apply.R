# This script tests the function parallel.apply
# This is the test file.
context("Function parallel.apply")

# Define the test data set dimensions

for (progress.bar in c(T, F)) {
  for (input in c('array', 'matrix')) {

    # Generate input
    if (input == 'array') {

      dims <- c(10, 20, 5, 8)
      X <- array(data = runif(prod(dims)), dim = dims)

    } else if (input == 'matrix') {

      dims <- c(100, 300)
      X <- matrix(data = runif(prod(dims)), nrow = dims[1], ncol = dims[2])

    } else {
      stop('Wrong input type')
    }

    # Randomly assign special values
    count <- length(X) * 0.01
    X[sample.int(length(X), size = count)] <- NA
    X[sample.int(length(X), size = count)] <- -Inf
    X[sample.int(length(X), size = count)] <- Inf

    # Define some other test variables
    FUN <- mean
    cores <- 2

    for (m in seq_len(length(dims))) {
      combns <- combn(x = seq_len(length(dims)), m = m)

      for (i.col in 1:ncol(combns)) {

        MARGIN <- combns[, i.col]

        # Carry out the test
        ret <- parallel.apply(
          X, MARGIN, FUN, na.rm = T, cores = cores,
          progress.bar = progress.bar)
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
  }
}

succeed('Tests passed for the function parallel.apply!')
