#
#
# Project Euler    // Problem 6:
# Summation of primes
#
#

# Problem blurb:
# =================================================
# The sum of the primes below 10 is;
# 2 + 3 + 5 + 7 = 17.
#
# Find the sum of all the primes below two million.
# =================================================

# The brute force approach to answering this problem
# would be to take each number below 2e6 in turn
# dividing it by all intgers from 2 up to it's sqrt.
# If any have no remainder it's not prime, otherwise
# prime. This however wastes a lot of compute time.

# Let's look at it from the other perspective. 2 is
# the first prime. Therefore all multiples of 2 are
# not prime. If we strike out all multiples of 2 as
# being not prime, then the next number on our list
# is prime (i.e. 3) and all multiples of this number
# are not prime.

# We can go on like this until we reach the end of
# our list which in this case is 2e6. But wait...
#
# We can save more time. We don't need to do this
# for every number in the list. We only need to run
# up to the sqrt of the number. Intuitively, if there
# is a factor of the number we are looking at that is 
# less than the sqrt of that number, then the other
# factor MUST be greater. So we just run this
# procdure on the smaller half of possible factors.

# Turns out this was thought up in 246BC and is called
# a Sieve of Erastothenes
"http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes"

esieve <- function( x ){ 
  candidates <- seq.int( 2 , x , 1 )
	nprime <- logical( length = length( candidates ) ) # Vector indicating whether a number is NOTPRIME (default = FALSE)
	end <- floor( sqrt( x ) )
	for( i in candidates[ 1:end ] ){
		if( !nprime[ (i-1) ] ){
			nprime[ ( c( seq.int( ( i*2 ) , x , i ) ) - 1 ) ] <- TRUE
			}
		}
	ans <- paste("Sum of primes below " , x , " is: " , sum( candidates[ !nprime ] ) , sep = "" )
  return( ans )
}



> microbenchmark( esieve(2e6) , times = 10 )
Unit: milliseconds
           expr      min       lq   median       uq      max
1 esieve(2e+06) 367.6846 370.1273 373.5495 388.0954 425.7716



# Takes about 0.37 seconds. I think I can improve this considerably.
# For starters we don't need to think about any of the even numbers.
# Also we might want to adapt it to use really big numbers in the
# future. For this a Sieve of Erasothenes might not be appropriate.
# Consider an Atkins Sieve....



# UPDATE WITH SOME MINOR IMPROVEMENTS HERE
