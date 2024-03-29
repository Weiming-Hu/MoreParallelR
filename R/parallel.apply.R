# "`-''-/").___..--''"`-._
#  (`6_ 6  )   `-.  (     ).`-.__.`)   WE ARE ...
#  (_Y_.)'  ._   )  `._ `. ``-..-'    PENN STATE!
#    _ ..`--'_..-_/  /--'_.' ,'
#  (il),-''  (li),'  ((!.-'
#
# Author: Weiming Hu <weiming@psu.edu>
#         Geoinformatics and Earth Observation Laboratory (http://geolab.psu.edu)
#         Department of Geography and Institute for CyberScience
#         The Pennsylvania State University

#' MoreParallelR::parallel.apply
#'
#' @description
#' MoreParallelR::parallel.apply provides a convenient solution
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
#' This idea was originally inspired by my advisor,
#' [Prof. Guido Cervone](http://geoinf.psu.edu/), during a
#' casual conversation.
#'
#' This function is different from
#' [plyr::laply](https://www.rdocumentation.org/packages/plyr/versions/1.8.4/topics/laply)
#' that it returns an array with the specified `MARGIN` as
#' dimensions.
#'
#' @note
#' Please be aware of whether your `FUN` behaves
#' differently for a vector, a matrix, or an array. If you
#' are applying the function on a matrix or an array, `lapply`
#' and `plyr:laply` will coerce the high-dimensional object
#' to vector; but `parallel.apply` will take the data **AS IT IS**
#' to feed the `FUN`. This might cause different results
#' from this function and `apply`.
#'
#' @param X An array, including a matrix.
#' @param MARGIN A vector giving the subscripts which the
#' function will be applied over.
#' @param FUN The function to be applied.
#' @param ... Optional arguments to `FUN`.
#' @param verbose Whether to print progress information.
#' @param cores The number of cores for parallelization.
#' @param progress.bar Whether to show a progress bar.
#' This requires the package `pbmcapply`.
#'
#' @return An array.
#'
#' @examples
#' # This example shows you how to run parallel.apply on a synthetic
#' # array and the how the performance compares to a serial run.
#' #
#'
#' library(profvis)
#'
#' profvis({
#'   library(MoreParallelR)
#'   library(magrittr)
#'
#'   # Generate synthesized data
#'   dims <- c(80 , 90, 100, 15)
#'   X <- dims %>%
#'     prod() %>%
#'     runif(min = 1, max = 10) %>%
#'     array(dim = dims)
#'
#'   MARGIN <- c(2, 4)
#'   cores <- 4
#'   FUN <- function(v) {
#'     library(magrittr)
#'
#'     # A costly function
#'     ret <- v %>%
#'       as.vector() %>%
#'       sin() %>%
#'       cos() %>%
#'       var()
#'
#'     return(ret)
#'   }
#'
#'   # Run the paralle code
#'   X.new.par <- parallel.apply(
#'     X, MARGIN, cores = cores, FUN)
#'
#'   # Run the serial code
#'   X.new.sq <- apply(X, MARGIN, FUN)
#'
#'   # Compare results
#'   identical(X.new.par, X.new.sq)
#' })
#'
#' @md
#' @export
parallel.apply <- function(
  X, MARGIN, FUN, ...,
  verbose = F, cores = 1, progress.bar = F) {

  # Sanity checks
  FUN <- match.fun(FUN)
  stopifnot(is.array(X) || is.matrix(X))
  stopifnot(max(MARGIN) <= length(dim(X)))

  if (progress.bar) {
    if (!requireNamespace('pbmcapply', quietly = T)) {
      stop(paste('Please install pbmcapply'))
    }
  } else {
    if (!requireNamespace('parallel', quietly = T)) {
      stop(paste('Please install parallel'))
    }
  }

  # Convert the high-dimensional array to a list of low-dimensional arrays
  if (verbose) cat('Splitting the array into a list of arrays ...\n')

  # This is the template for indexing values. This will be used by later codes.
  index.paradigm <- lapply(dim(X), seq_len)

  # Create the index mapping from list to array
  dimensions <- index.paradigm[MARGIN]
  index.loop <- expand.grid(dimensions)
  index.loop <- as.matrix(index.loop)

  # Slice the array
  X.list <- vector(mode = 'list', length = nrow(index.loop))
  for (i.row in 1:length(X.list)) {

    # Define indexing
    index <- index.paradigm
    index[MARGIN] <- as.list(index.loop[i.row, ])

    # Copy values
    X.list[[i.row]] <- do.call('[', c(list(X), index, list(drop = F)))
  }

  # Parallel processing the list of array
  if (verbose) cat('Processing ...\n')

  if (progress.bar) {
    ret <- pbmcapply::pbmclapply(
      X = X.list, FUN = FUN, ..., mc.cores = cores)
  } else {
    ret <- parallel::mclapply(
      X = X.list, FUN = FUN, ..., mc.cores = cores)
  }

  if (verbose) cat('Organizing values back to an array ...\n')
  X.new <- array(NA, dim = dim(X)[MARGIN])
  for (i.row in 1:nrow(index.loop)) {
    index <- index.loop[i.row, , drop = F]
    X.new[index] <- ret[[i.row]]
  }

  # Housekeeping
  rm(i.row, index, ret, X.list, index.loop)
  garbage <- gc(reset = T)

  if (verbose) cat('Done (parallel.apply)!\n')
  return(X.new)
}
