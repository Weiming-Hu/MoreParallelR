# This script tests the function array.apply
# This is the test file.
context("Function array.apply")

verbose <- F

# Define the test data set dimensions
dims <- c(10, 20, 10, 15)
X <- array(data = runif(prod(dims)), dim = dims)

# Randomly assign special values
count <- length(X) * 0.01
X[sample.int(length(X), size = count)] <- NA
X[sample.int(length(X), size = count)] <- -Inf
X[sample.int(length(X), size = count)] <- Inf

# Define some other test variables
FUN <- mean
cores <- 2

# This is the first group of tests
if (verbose) cat('test.array.apply group 1 ...\n')
for (MARGIN in 1:length(dims)) {

  # Carry out the test
  ret <- array.apply(
    X, MARGIN, cores, FUN, na.rm = T, verbose = F)
  ret.serial <- apply(
    X, MARGIN, FUN, na.rm = T, verbose = F)

  # Check results
  stopifnot(identical(as.vector(ret.serial), as.vector(ret)))
}

# This is the second group of tests
if (verbose) cat('test.array.apply group 2 ...\n')
for (leave.out.MARGIN in 1:length(dims)) {
  MARGIN <- 1:length(dims)
  MARGIN <- MARGIN[-leave.out.MARGIN]

  # Carry out the test
  ret <- array.apply(
    X, MARGIN, cores, FUN, na.rm = T, verbose = F)
  ret.serial <- apply(
    X, MARGIN, FUN, na.rm = T, verbose = F)

  # Check results
  stopifnot(identical(ret.serial, ret))
}

succeed('Tests passed for the function array.apply!')
