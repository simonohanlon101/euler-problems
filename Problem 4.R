#
#
# Project Euler  	// Problem 4:
# What is the largest palindromic product of two 3 digit numbers
#
#

# Problem blurb:
# ==============================================
# A palindromic number reads the same both ways.
# The largest palindrome made from the product
# of two 2-digit numbers is 9009 = 91 * 99
# ==============================================


# As this function is written n must be 3 or the palindrome matching will fail
# An improved algo is below, which also avoids using matrices so that n can
# get larger

palindrome_prod <- function( n = 3 ){
  x <- 10^( n - 1 ) : ( 10^(n) - 1 )
  m <- outer( x , x )
  m <- sort( as.vector( m ) , decreasing = TRUE )
  res <- ( m[ grepl( '^([0-9])([0-9])([0-9])\\3\\2\\1$', m ) ] )[1]
  return( res )
  }


# It's a bit inefficient to sort the entire matrix of products.
# Especially if n begins to get larger. There are some
# improvements we can make:
#
# 1) Extract the lower or upper triangle of the matrix only as
#    it's symmetric. This is the same as for each element of x,
#    creating a vector of x multiplied by itself and every greater
#    element of x
#
# 2) Match all palindromes for each vector of x
#
# 3) Use max() to find the largest of these matches


# The more flexibleand faster algorithm then becomes:

palindrome_prod2 <- function( n ){
  x <- 10^( n - 1 ) : ( 10^(n) - 1 )
  pal2 <- function( x , n ){
  y <- x * x:( 10^(n) - 1 )
  s <- paste( rep( paste("([0-9])" , sep = "" ) , n) , collapse = "" )
  e <- paste("\\" , n:1 , sep = "" , collapse = "" )
  pat <- paste( "^" , s , e , "$" , sep = "" , collapse = "" )
  if( any( grepl( pat , y ) ) ){
      res <- max( y[ grepl( pat , y ) ] , na.rm = TRUE )
  }else{
  	res <- NA
  }
  return( res )
  } 
  mat <- max( sapply( x , FUN = pal2 , n = n ) , na.rm = TRUE )
  return(mat)
  }



# Some timings for the two algo's for n = 2, and then n = 3 (10 replications each):
> microbenchmark( palindrome_prod(3) , palindrome_prod2(3) , times = 10 )
Unit: seconds
                 expr      min       lq   median       uq      max
1  palindrome_prod(3) 2.188919 2.220579 2.245376 2.258202 2.330388
2 palindrome_prod2(3) 1.082289 1.147273 1.170697 1.175940 1.190959


