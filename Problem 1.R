#
#
# Project Euler    // Problem 1:
# Find the sum of all the multiples of 3 or 5 below 1000
#
#



x <- 1:999
sum( x[ x %% 5 == 0 | x %% 3 == 0 ] )



# Another solution (this is really bad R practice!) could be:

x <- 1:999
j <- 0
for( i in x ){ 
  if( x[i]%%3==0 || x[i]%%5==0 ){
		y <- x[i]
		j = j + y
		}
	}



# With 100 numbers this second solution is approximately 7 times slower than the first
# With 1000 numbers this rises to about 18
# And with 1e5 numbers? 30 times. The larger the set, the greater the difference in inefficiency
# Who cares with a runtime of <0.1s?
# Well, what if it's a billion? What if there are more complex operations involved than modulo division and addition?
# Lesson: take advantage of R's vectorisation for speed.
