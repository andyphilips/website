/* Andy Philips
   Texas A&M Univ.
   Last Updated: 7/27/14
   
 * -----------------------------------------------------------------------

 Monte Carlos: Structural Breaks:


Do structural breaks change the performance of simple unit root tests? To examine this, we can create a Monte Carlo analysis. Consider a non-stationary series given as a random walk w/ drift and trend:
 
			y_t =  alpha +  y_t-1 + beta*t + epsilon_t
 	but say we have a structural break in trend where the trend becomes
 	 beta2*t after the break point t* > t:
 			y_t =  alpha +  y_t-1 + beta2*t + epsilon_t
 */
 
clear

set seed 584093

set obs 100
gen t = _n
tsset t
scalar alpha = 100
scalar beta = .01
gen y =  alpha + rnormal()
replace y =   l.y  + beta*t + rnormal() in 2/100
tsline y


* here is one with a structural break in trend at t = 50 
scalar gamma = -.02
gen y2 = alpha + rnormal()
gen breakpt = 0
replace breakpt = 1 if t >49
replace y2 =   l.y2 + beta*t + gamma*t*breakpt + rnormal() in 2/100

tsline y y2
tsline d.y d.y2

dfuller y2
dfuller d.y
dfuller d.y2

zandrews y2, graph break(trend) trim(0.15)
zandrews y2, graph break(trend) trim(0.24)



* here is one for a structural break in intercept
gen yi = sum(rnormal())
replace yi = yi - 5 in 50
replace yi = l.yi + rnormal() in 51/100
tsline yi, xline(50)
zandrews yi, graph trim(0.15) break(both) lagmethod(TTest)
zandrews yi, graph trim(0.15) break(both) lagmethod(AIC)



*	------------------------------------------------------------------
*	MAKE A MONTE CARLO FOR A BREAK IN INTERCEPT:

* We explore the t-min and estimated location of the strucutral break in 
* intercept under the Z-Andrews test. 
* we change the size of the intercept break from -10 to 10 by 1 unit increments.

clear 
set seed 40592
global observ = 120

*	get various inputs for the MC:
global nmc = 1000
global breakpt = $observ/2 // make the location of the break point right 
*	in the middle
set obs $observ 
gen time = _n
tsset time
gen y = .	// generate up here to save computing time


cap prog drop ibreak
program ibreak, rclass
tempname sim
postfile `sim' breaksize location tmin using i_break_results, replace

*	-------
forv m = 1/$nmc	{				// # of monte carlo simulations
	qui forv q = 1/20	{			// 
		local breaksize = `q'-10	// breakpoint size from -10 to 10
		replace y = sum(rnormal())	// create random walk	
		replace y = y + breaksize if time == $breakpt	// add the breakpt
		replace y = l.y + rnormal() if time > $breakpt
		qui zandrews y, 			// run the test
		scalar tmin = r(tmin)		// grab the t statistic
		scalar location = r(tminobs)	// grab where this min(tstat) is
		post `sim' (`breaksize') (location) (tmin)	// post it to file
	}		
}	
postclose `sim'	
end
		
	
	
*	Now run it:
ibreak


* let's do some post simulation graphs. Our simulation saved the estimates
* in "i_break_results.dta" :
cd "/Users/andyphilips/Documents/Methods/Monte Carlo/"

use "i_break_results", clear
collapse (mean) location tmin, by(breaksize)
twoway connected location breaksize, yline(60)
* there doesn't seem to be any positive vs. negative difference in break-pattern
* in fact it is spot on when the breaksize is small.


use i_break_results, clear
twoway kdensity tmin if breaksize == -9 || kdensity tmin if breaksize == -2 || kdensity tmin if breaksize == 2 || kdensity tmin if breaksize == 9, legend( order(1 "breaksize = -9" 2 "breaksize = -2" 3 "breaksize = 2" 4 "breaksize = 9"))
* larger breaksizes get us slightly smaller t statistics

* generate squared deviations from TRUE break (60)
gen sdft = abs(location - 60)^2
twoway kdensity sdft if breaksize == -9 || kdensity sdft if breaksize == -2 || kdensity sdft if breaksize == 2 || kdensity sdft if breaksize == 9, legend( order(1 "breaksize = -9" 2 "breaksize = -2" 3 "breaksize = 2" 4 "breaksize = 9"))
* Squared deviations are the smallest when the breaksize is large.

collapse (mean) location tmin sdft, by(breaksize)
twoway connected sdft breaksize, yline(0)
