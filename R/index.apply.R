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

#' MoreParallelR::index.apply
#'
#' @description
#' MoreParallelR::index.apply makes it convenient to parallel
#' processing with multiple arrays. Sometimes, the parallelization
#' becomes hard because you need to access index (positions)
#' during the process. This function is designed specifically
#' for this situation.
#'
#' @author Weiming Hu
#'
#' @details
#' The way this has been done is that a list of arrays are
#' expected from the user and the MARGIN dimensions should be
#' identical because those are the dimensions to be partitioned.
#' All members in the list will be partitioned and put into
#' a separate list. The first member in this list is the
#' corresponding index for the MARGIN dimensions and the
#' subsequent members in this list are the input partitioned arrays.
#'
#' The difficulty might be how to write FUN. It should be
#' a function which takes one list with the partitioned arrays
#' and any other extra arguments. If you want to examine
#' the partitioned list, it is suggested to use `debug = T` before
#' you write your FUN.
#'
#' To see better improvement by the parallelization, it is
#' preferred to have the runtime of `FUN` longer. In other
#' words, this solution works better when you have a heavy
#' workload in the function `FUN`.
#'
#' @param X A list of arrays or matrices.
#' @param MARGIN A vector giving the subscripts which the
#' function will be applied over. Slices from all elements
#' in the input list `X` will be created.
#' @param FUN The function to be applied. The first argument
#' of this function should take a member from the list that is
#' constructed internally by this function. Please use
#' `debug = T` to examine this internally constructed list.
#' @param ... Additional arguments to `FUN`.
#' @param debug Set this to TRUE to return the internally
#' constructed list. This is helpful when you are designing
#' your function.
#' @param verbose Whether to print progress information.
#' @param cores The number of cores for parallelization.
#' @param progress.bar Whether to show a progress bar.
#' This requires the package `pbmcapply`.
#'
#' @return An array.
#'
#' @examples
#' # Imagine that you have 2 arrays with different dimensions.
#' a1 <- array(1:1000, dim = c(10, 5, 20))
#' a2 <- array(1:2000, dim = c(10, 10, 20))
#'
#' # You have a constant which will be involved
#' # during the calculation.
#' #
#' c <-  5.5
#'
#' # You have a function that you would like to iterate
#' # on the first and third dimensions of the arrays.
#' #
#' foo <- function(x, y, c) {
#'   return(mean(x) + mean(y) + c)
#' }
#'
#' # To write the sequential version, we need to preallocate
#' # a new array and the required memory.
#' #
#' d <- array(NA, dim = c(10, 20))
#'
#' # We need to write a nested for loop because we want
#' # to apply the function on two arrays based on the iteration
#' # on the first and the third dimensions.
#' #
#' for (i in 1:10) {
#'   for (j in 1:20) {
#'     d[i, j] <- foo(a1[i, , j], a2[i, , j], c)
#'   }
#' }
#'
#' # To use the index.apply function, we need to put
#' # our arrays in a list.
#' #
#' X <- list(a1, a2)
#'
#' # Define our margin
#' MARGIN <- c(1, 3)
#'
#' # Define the functions. The first argument should be
#' # an element from the list which you can get by running
#' # the following code with `debug = T`.
#' #
#' internal.list <- index.apply(X, MARGIN, 4, NULL, debug = T)
#'
#' # The returned value is a list with length of 200, because
#' # the iteration is carried out on the first (length of 10)
#' # and third (length of 20) dimensions. Each element in this
#' # list will be fed into the function as the first argument.
#' # So you should take a look at the element in order to design
#' # your function accordingly.
#' #
#' length(internal.list)
#'
#' # Each element is a list with the iteration index as the
#' # first member, and then the sliced arrays/matrices with
#' # these indices as the following members.
#' #
#' length(internal.list[[1]])
#' names(internal.list[[1]])
#'
#' # Therefore, we design our function accordingly.
#' FUN <- function(l, c) {
#'   return(mean(l[[2]]) + mean(l[[3]]) + c)}
#'
#' # Run the same calculation with index.apply.
#' d.new <- index.apply(X, MARGIN, FUN = FUN, c = c, cores = 2)
#'
#' # Check
#' identical(d, d.new)
#'
#' @md
#' @export
index.apply <- function(
  X, MARGIN, FUN, ...,
  verbose = F, debug = F,
  cores = 1, progress.bar = F) {

  # Sanity checks
  if (progress.bar) {
    require(pbmcapply)
  } else {
    require(parallel)
  }

  # Input X should be a list
  stopifnot(is.list(X))
  stopifnot(length(X) >= 1)

  # Each element in the list should be either an array or a matrix
  if (!all(sapply(X, function(x) {
    return(is.array(x) || is.matrix(x))}))) {
    stop('Elements in X should be either array or matrix.')
  }

  # All element in the list should have the same dimension for MARGIN
  if (length(unique(sapply(X, function(x) {
    return(paste(dim(x)[MARGIN], collapse = ';'))}))) != 1) {
    stop("Elements in X should have the same dimensions for MARGIN.")
  }

  # Convert the high-dimensional array to a list of low-dimensional arrays
  if (verbose) cat('Splitting the array into a list of arrays ...\n')

  index.paradigm <- lapply(X, function(x) {
    return(lapply(dim(x), seq_len))})

  grid.table <- expand.grid(lapply(
    MARGIN, function(x) {
      return(1:dim(X[[1]])[x])}))

  grid.table <- as.matrix(grid.table)

  X.dist <- vector(mode = "list", length = nrow(grid.table))

  for (i.row in 1:length(X.dist)) {

    X.dist[[i.row]] <- list(
      index = grid.table[i.row, ])

    index <- index.paradigm

    for (i.X in 1:length(X)) {
      index[[i.X]][MARGIN] <- as.list(grid.table[i.row, ])
      X.dist[[i.row]][[paste0('array', i.X)]] <- do.call(
        "[", c(list(X[[i.X]]), index[[i.X]], list(drop = F)))
    }
  }

  if (debug) {
    return(X.dist)
  }

  FUN <- match.fun(FUN)

  if (verbose) cat('Processing ...\n')
  if (progress.bar) {
    ret <- pbmcapply::pbmclapply(
      X = X.dist, FUN = FUN, ..., mc.cores = cores)
  } else {
    ret <- parallel::mclapply(
      X = X.dist, FUN = FUN, ..., mc.cores = cores)
  }

  if (verbose) cat('Organizing values back to an array ...\n')

  X.new <- array(NA, dim = dim(X[[1]])[MARGIN])

  for (i.row in 1:nrow(grid.table)) {
    index <- grid.table[i.row, , drop = F]
    X.new[index] <- ret[[i.row]]
  }

  if (verbose) cat('Done (index.apply)!\n')
  return(X.new)
}
