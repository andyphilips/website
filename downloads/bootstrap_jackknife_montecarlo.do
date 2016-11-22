* Andy Philips
* Texas A&M
* 12/27/2013
*
*	Bootstrapping, Jack-knifing, and Monte Carlo
*
*	-----------------------------------------------------------------------
*	This .do file contains code mostly for the canned stata procedures
*	for bootstrap, jackknife, and monte carlos. Examples are given from 
*	generated data

* DGP:
clear
set seed 345
set obs 120
gen e1 = rnormal()
gen x1 = rnormal()
gen x2 = rnormal()
gen x3 = rnormal()
gen y = 2*x1 + 3*x2 + e1









* BOOTSTRAPPING	--------------------------------------------------------------
/* bootstrapping standard errors from a statistic can be used by the following:
	1. write program (if you have a custom statistic program)
	2. load in data
	3. drop missing values (STATA will not discern if you have missing 
	values)
	4. drop unneeded variables (this speeds up a bootstrap)
	5. set seed
	6. run bootstrap
*/

* first drop any missing obs
foreach var in y x1 x2 {
	drop if `var' == .
}

reg y x1 x2
bootstrap, reps(1000): regress y x1 x2

/* note how the t-scores are slightly lower in the bootstrap, but the coeffs. 
	remain the same. for speed we can drop unneeded vars.
	
*/
timer clear
timer on 1
bootstrap, reps(1000): regress y x1 x2
timer off 1

timer on 2
drop x3
bootstrap, reps(1000): regress y x1 x2
timer off 2
timer list 
* we get moderate gains even with only 120 obs and 4 variables


* Jack-knifing ---------------------------------------------------------------
jackknife coef=_b[x1]: reg y x1 x2

/* bootstrap is more efficient than JK-ing, but jackknife is still good at 
 getting influential observations. first let's add an influential point
*/
replace x1 = 30 in 20
hist x1

* now we add a standard deviation jknife and skewness, keeping the variables
jackknife sd=r(sd) skew=r(skewness), rclass keep: summarize x1, det
summarize sd skew, detail 

list x1 if sd > 10


* Monte Carlo Simulation -----------------------------------------------------

* MCs simulate finite data from a hypothesized model...usually we want to 
* examine our confidence intervals to verify that they are the `true' intervals

* first, lets program a simple regression where we can set the observations
* and some c that changes the x vars
capture program drop myreg
program myreg
	version 12
	clear
	args obs c
	set obs `obs'
	gen x1 = rnormal()
	gen x2 = rnormal()
	gen y = 2*x1*`c' + 3*x2*`c' + rnormal()
	regress y x1 x2
end

* now the monte carlo part.  we specify 100 observations and a coefficient 
* `changer' of 1
set seed 345
simulate _b _se, reps(1000) nodots: myreg 100 1
su

simulate _b _se, reps(1000) nodots: myreg 1000 1
su




* to see what's going on, another way to do so is to hand-write our program:
clear
set seed 2355
cd "/Users/andyphilips/Documents/Methods/Stata Help"
set obs 100
* we can set our values as before:
scalar constant = 0
scalar x1slope = 2
scalar x2slope = 3
scalar sigma = 1
scalar alpha = 0.05

* now we make up our equation
gen x1 = rnormal()
gen x2 = rnormal()
gen y_true = constant + x1slope*x1 + x2slope*x2

gen epsilon = .
gen y = .
tempname simulation
* how many reps you want
global reps = 1000

* now we can loop our program
postfile `simulation' b1 b2 se1 se2 coverage1 coverage2 using results, replace
	quietly {
		forv i = 1/$reps {
			replace epsilon = rnormal(0,sigma)
			replace y = y_true + epsilon
			reg y x1 x2
			scalar b1 = _b[x1]
			scalar b2 = _b[x2]
			scalar se1 = _se[x1]
			scalar se2 = _se[x2]
			scalar lb1 = b1 -se1*invttail(e(df_r),alpha/2)
			scalar ub1 = b1 +se1*invttail(e(df_r),alpha/2)
			scalar lb2 = b2 -se2*invttail(e(df_r),alpha/2)
			scalar ub2 = b2 +se2*invttail(e(df_r),alpha/2)
			scalar pv1 = x1slope<ub1 & x1slope>lb1
			scalar pv2 = x2slope<ub2 & x2slope>lb2
			post `simulation' (b1) (b2) (se1) (se2) (pv1) (pv2)
		}
	}
	
postclose `simulation'

use results, clear 
summarize
			







