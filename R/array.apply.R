#' MoreParallelR::array.apply
#'
#' @description
#' MoreParallelR::array.apply provides a convenient solution
#' for parallelizing the `apply` function on array. This
#' function first breaks the dimension specified by `MARGIN`
#' to a list of smaller arrays and then call the `mcapply`
#' function to achieve the rest of the parallelization.
#'
#' @author Weiming Hu
#'
#' @details
#' To see better improvement by the parallelization, it is
#' preferred to have the runtime of `FUN` longer. In other
#' words, this solution works better when you have a heavy
#' workload in the function `FUN`.
#'
#' @param X An array.
#' @param MARGIN A vector giving the subscripts which the function will be applied over.
#' @param FUN The function to be applied.
#' @param ... Optional arguments to `FUN`.
#' @param verbose Whether to print progress information.
#'
#' @return An array.
#'
#' @md
#' @export
array.apply <- function(X, MARGIN, cores, FUN, ..., verbose = T) {

  # Sanity checks
  stopifnot(is.array(X))
  stopifnot(max(MARGIN) <= length(dim(X)))

  require(parallel)

  # Convert the high-dimensional array to a list of low-dimensional arrays
  if (verbose) cat('Splitting the array into a list of arrays ...\n')

  # Create the iterating numbers for specified dimensions
  iterate.list <- list()
  for (i in 1:length(MARGIN)) {
    iterate.list[[i]] <- 1:dim(X)[MARGIN[i]]
  }

  # Create all combinations of indices for specified dimensions
  index.loop <- expand.grid(iterate.list)
  index.loop <- as.matrix(index.loop)

  # This is the template for indexing values. This will
  # be used by later codes.
  #
  index.paradigm <- c(rep(',', times = length(dim(X))), 'drop = F')

  X.list <- vector(mode = 'list', length = nrow(index.loop))
  for (i.row in 1:length(X.list)) {

    index <- index.loop[i.row, ]

    # Construct the indexing for this single dimension
    index.single <- index.paradigm
    for (i.col in 1:length(index)) {
      index.single <- append(
        x = index.single,
        values = index[[i.col]],
        after = which(index.single == ',')[MARGIN[i.col]] - 1)
    }

    index.single <- paste(index.single, collapse = '')
    index.single <- paste0('X[', index.single, ']')

    # Copy values
    X.list[[i.row]] <- eval(parse(text = index.single))
  }

  # Parallel processing the list of array
  ret <- mclapply(X = X.list, FUN = FUN, ..., mc.cores = cores)

  if (verbose) cat('Organizing values back to an array ...\n')
  X.new <- array(NA, dim = dim(X)[MARGIN])
  for (i.row in 1:nrow(index.loop)) {

    index <- index.loop[i.row, , drop = F]
    X.new[index] <- ret[[i.row]]
  }

  # Housekeeping
  rm(i.row, index, ret, X.list, index.loop)
  garbage <- gc(reset = T)

  if (verbose) cat('Done (array.apply)!\n')
  return(X.new)
}
