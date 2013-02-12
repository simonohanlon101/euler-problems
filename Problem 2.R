#
#
# Project Euler  	// Problem 2:
# What is the sum of all even Fibonacci numbers below 4e6?
# 
#



# Function to generate Fibonacci numbers up to some limit and sum the even numbers in that sequence
# Running time for numbers up to 4 million is ~ 5 to 10 milliseconds (0.005 to 0.001s)


fib <- function( lim ){
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

# I was going to try optimising this a bit, but the difference in speed
# between running this algo for a limit of 1,000 or 2,000,000,000 is
# <100 microseconds (0.1 ms or 100 millionths of a second).

# 100 replications:
> microbenchmark( fib( 1e3 ) , fib( 2e9 ) )
Unit: microseconds
        expr     min       lq   median       uq     max
1  fib(1000)  55.561  56.5085  57.1555  58.3440 122.431
2 fib(2e+09) 139.825 143.0490 145.2175 147.1095 235.493



# Even still let's 'R-ise', it using some mathematical identities.....

fib2 <- function( lim , d ){
	require( Rmpfr )
	lim <- mpfr( lim , 1024 )
	n <- 1:floor(2.0801 * as.integer( log( lim ) ) + 2.1408) 	# There are n terms in Fibonnaci sequence below limit given by this equation
    phi = (1 + sqrt(5)) / 2	
	ans <- floor( mpfr( phi^n , 1024 ) / sqrt(5) + 1/2 )		# f(n) for each n in sequence
	ans <- sum( ans[ ans %% d == 0 ] )			# Use vectorisation to select numbers evenly divisible by the given divisor
	return( paste( "The sum of even Fibonnaci numbers below " , lim , " is " , ans , sep = "" ) )
}



> microbenchmark( fib( 2e9 ) , fib2( 2e9 , 2 ) , times = 1000 )
Unit: microseconds
            expr     min       lq   median       uq      max
1     fib(2e+09) 138.622 142.6360 144.2495 149.9985 1581.101
2 fib2(2e+09, 2)  29.201  30.2025  30.6380  31.7040 5418.810



# Around a five-times speedup, plus we can choose the divisor we want to test against.
# The only other alterations I might make are some code to allow it to run on limits larger
# than ~2.147 billion (limits of R integer calculations). We can use the 'gmp' package
# for arbitrarily BIGGGGG numbers like so...


fib3 <- function( lim = "1" , d ){
	require( gmp )
	"%e%" <- function( a , e ){
		as.bigz( a ) * 10^as.bigz( e )
		}
	if( grepl( "[e]" , as.character( lim ) ) ){
		sn <- unlist( strsplit( as.character( lim ) , "e" , fixed = TRUE ) )
		lim <- sn[1] %e% as.integer( sn[2] )
		}else{
			lim <- as.bigz( lim )
		}
	n <- seq( from = 1 , to = ( 2.0801 * log( lim ) + 2.1408 ) )
	ans <- 0
	for( i in n ){
		fib <-  fibnum( i )
		if( mod.bigz( fib , d ) == 0 ){
			ans <- add.bigz( ans , fib )
		}
	}
	return( paste( "The sum of even Fibonnaci numbers below " , lim, " is " , ans , sep = "" ) )
}



# This algo is around 100 times slower, BUT as 0.1s does it really matter?
# Plus we can calculate for really big limits....


> fib3( 30 , 2 )
[1] "The sum of even Fibonnaci numbers below 30 is 44"
> fib3( 4e6 , 2 )	# Same as first algo
[1] "The sum of even Fibonnaci numbers below 4000000 is 4613732"
> fib3( "1e1000" , 2 )	# Greater than largest double
[1] "The sum of even Fibonnaci numbers below 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 is 106474282045905002360639071992669274044874327808157975788083009080486034737584200963614626396895471004191009021609860801002262899279990284935686456950133879097857711452758225190880362518607867020605274078296774158710888995170180330539073893090576955802004552790670565923385215657964819982964917592833610537155312749129881782241146488329434361878607766523404516568956297346750376013079353562419829958154233072880422868524107707843766520685355030458453264501004266386077736513802847547511032148647465465386005381189684959489233700062046624747702492971986764237016308687530865358241739363550831914754822239608183975970853406877485364220869767554080725457032695755874069715128329175417336115389771691574254857069775112393935074387230457053991376763594721690699965568770571899889842355244385345969623060206888775139269998138115638067263711558121601501052541872351166287102413165623547114630191265423554103028582632512402760938881948386893744326403189494671971490876577195550234268089136493600352016252766406"




