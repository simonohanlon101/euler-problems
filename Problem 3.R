#
#
# Project Euler    // Problem 3:
# Find largest prime factor of 600851475143
# 
#



p_fac <- function( p ){
  pfs <- integer()
	while( p > 1 ){
		d <- 2
		x <- p %% d
		while( x != 0 && d < p ){
			d <- d + 1
			x <- p %% d 
		}
		pfs <- c( pfs , d )
		p <- p / d
	}
	return( pfs )
}

# Actually this function returns all prime factors of a number which I think is more interesting
# For Project Euler just pick the largest number in returned list or try
# max( p_fac( 600851475143 ) )
# Runs in ~ 23 milliseconds (0.023s) for 600851475143
