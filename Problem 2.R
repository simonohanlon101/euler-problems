#
#
# Project Euler  	// Problem 2:
# What is the sum of all even Fibonacci numbers below 4e6?
# 
#



# Function to generate Fibonacci numbers up to some limit and sum the even numbers in that sequence
# Running time for numbers up to 4 million is ~ 5 to 10 milliseconds (0.005 to 0.001s)


even_fib <- function( lim ){
    a <- 1
    b <- 1
    ans <- 0
    while( b < lim ){
        c <- a + b
		a <- b
	    b <- c
		if( c %% 2 == 0 ){
		    ans <- ans + c
		}	
	}
	return( paste( "The sum of even Fibonnaci numbers below " , lim , " is " , ans , sep = "" ) )
}

# > even_fib( 4000000 )
# [1] "The sum of even Fibonnaci numbers below 4e+06 is 4613732"


