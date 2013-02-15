#
#
# Project Euler    // Problem 6:
# Sum square difference
#
#

# Problem blurb:
# ==============================================
# The sum of the squares of the first ten natural numbers is,
# 12 + 22 + ... + 102 = 385
#
# The square of the sum of the first ten natural numbers is,
# (1 + 2 + ... + 10)^2 = 55^2 = 3025
#
# Hence the difference between the sum of the squares of the
# first ten natural numbers and the square of the sum is,
# 3025 - 385 = 2640.
#
# Find the difference between the sum of the squares of the
# first one hundred natural numbers and the square of the sum.
# ==============================================


# This is trivial in R...

ssdif <- function( x ){

  ssum <- sum( 1:x )^2
  sums <- sum( (1:x)^2 )
  ans <- ssum - sums
  return( ans )

}

# It runs so fast and takes advantage of R's vectorisation using
# the underlying C primitives that there is probably not much we
# could do to speed it up.

> microbenchmark( ssdif(100) , ssdif(1000) , ssdif(1e4) , times = 100 )
Unit: microseconds
          expr     min       lq  median       uq       max
1   ssdif(100)   8.514   9.1660   9.768  10.4690    30.505
2  ssdif(1000)  23.315  24.5960  25.207  27.2095   204.373
3 ssdif(10000) 165.158 176.3255 188.043 258.8560 19956.399


# You could make it more extensible for when x gets quite large 
# As it is written you will get an integer overflow error for 
# values of x > 65,535 (which is 2^32 - 1) and represents the 
# max value of an unsigned integer in R. The fix for this is to
# simply use sum( as.numeric( 1:x ) ) like so:


ssdif2 <- function( x ){

  ssum <- sum( as.numeric( 1:x ) )^2
  sums <- sum( as.numeric( (1:x)^2 ) )
  ans <- ssum - sums
  return( ans )

}


# Type conversion from integer to numeric adds a few millionths of a second
# on to the runtime, but we can now handle larger values of x

> microbenchmark( ssdif( 1000 ) , ssdif2( 1000 ) , times = 1000 )
Unit: microseconds
          expr    min     lq median     uq      max
1  ssdif(1000) 17.526 18.539 18.877 19.318 1636.654
2 ssdif2(1000) 21.050 22.147 22.591 23.176 1237.515

