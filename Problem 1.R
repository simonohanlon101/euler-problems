#
#
# Project Euler    // Problem 1:
# Find the sum of all the multiples of 3 or 5 below 1000
#
#


mult <- function( lim ){
    x <- 1:lim
    ans <- sum( x[ x %% 5 == 0 | x %% 3 == 0 ] )
    return( ans )
}


# Another solution (this is really bad R practice!) could be:

mult2 <- function( lim ){
    x <- 1:lim
    j <- 0
    for( i in x ){ 
        if( x[i] %% 3 == 0 || x[i] %% 5 == 0 ){
		y <- x[i]
		j = j + y
		}
	}
    return( j )
}


# The second algorithmn is approx 30 times slower than the 'well-formed' R code.
# But clearly it would only make a difference for a very long list of numbers


> microbenchmark( mult(999) , mult(9999) , mult2(999) , mult2(9999) , times = 100 )
Unit: microseconds
         expr       min        lq     median         uq       max
1   mult(999)   125.153   126.583   128.3715   145.5065   325.466
2  mult(9999)  1153.006  1171.145  1181.5590  1199.3400  2137.303
3  mult2(999)  2887.427  2970.345  3009.2695  3046.7090  4310.598
4 mult2(9999) 29534.328 30853.318 31043.5225 31304.8325 60043.597


# 128 microseconds is 0.000128 seconds, 31,000 microseconds is 0.031 seconds

