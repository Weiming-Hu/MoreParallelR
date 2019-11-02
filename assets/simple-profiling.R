library(profvis)

profvis({
	library(MoreParallelR)
	library(magrittr)
	library(future.apply)
	
	# Generate synthesized data
	dims <- c(80 , 900, 100, 15)
	X <- dims %>%
		prod() %>%
		runif(min = 1, max = 10) %>%
		array(dim = dims)
	
	MARGIN <- c(2, 4)
	cores <- 4
	FUN <- function(v) {
		library(magrittr)
		
		# A costly function
		ret <- v %>%
			as.vector() %>%
			sin() %>%
			cos() %>%
			var()
		
		return(ret)
	}
	
	# Run the paralle code
	X.new.par <- parallel.apply(
		X, MARGIN, cores = cores, FUN)
	
	# Run the paralle code with future
	plan(multisession)
	X.new.par.future <- future_apply(
		X, MARGIN, FUN)
	plan(sequential)
	
	# Run the serial code
	X.new.sq <- apply(X, MARGIN, FUN)
	
	# Compare results
	identical(X.new.par, X.new.sq)
	identical(X.new.par, X.new.par.future)
})