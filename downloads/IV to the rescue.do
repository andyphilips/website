* Andy Philips
* Last updated: 5/22/16
*  IV regression Monte Carlo: IV To The Rescue?

*	------------------------------------------------------------------------
/* Remember that the first-order moment condition of OLS is E[e|X] =0.
	What happens if there is a correlation between e and X? One solution
	is an Instrumental Variables regression. In this Monte Carlo, we Break
	our IV regression. There are 2 requirements of instruments:
		1. Relevance: Corr(x,z) != 0
		2. Exogeneity: Corr(z,e) ==0 
	We will examine what happens when we violate 1, and 2 through simulations
*/

clear
set seed 23555
* Set your directory:
cd "~/Monte Carlo"


* specify our correlation between the independent variable (x) and error (e)
* and correlate x and z
matrix C = (1, .5, .4 \ .5, 1, 0 \ .4, 0, 1)
* we can use drawnorm to generate data w/ the specified correlation structure
* that adds random sampling error (don't use corr2data which does not vary the
* samples from one to the next---thanks to Gueorgui Kolev for pointing this
* out!).
drawnorm x e z, n(120) corr(C)	

correlate x e z
* Notice that we have violated the first order moment condition,
* since E[e|X] !=0. But, our z instrument remains mostly uncorrelated with the 
* error term.

* our model takes the following form:
gen y = 1 + 2*x + e

reg y x
* notice how our x variable, correlated w/ e, is inconsistent and 
* biased upwards


* now instrument in 2 stage least squares 
ivregress 2sls y (x = z),
estat endogenous

* What is really going on:
reg x z
predict x_res, resid
reg y x x_res			
* note that although the coefficient on X is the same across both Stata's IV 
* procedure and our own one by hand, the standard errors in our model are 
* smaller; this is because we are not taking into account the uncertainty
* associated with x_res since it was produced in stage 1. Stata's procedure 
* does this automatically


* We can create a program to examine different ways to "break" the IV. We add
* three arguments (inputs) to the program, which specify the three relevant
* correlations. The program then takes these and constructs the correlation
* matrix, draws the data, runs the regression, and returns the relevant 
* results:
capture program drop myivreg
program myivreg, rclass
version 12
	drop _all
	args xzcorr xecorr zecorr
	return scalar xzcorr = `xzcorr'
	return scalar xecorr = `xecorr'
	return scalar zecorr = `zecorr'
	matrix C = (1, `xecorr', `xzcorr' \ `xecorr', 1, `zecorr'  \ `xzcorr', `zecorr' , 1)
	drawnorm x e z, n(120) corr(C)
	gen x2 = runiform()
	gen y = 1 + 2*x + 3*x2 + e
	ivregress 2sls y x2 (x = z)
	return scalar b = _b[x]
	return scalar se = _se[x]
	return scalar rmse = e(rmse)
	
end



* always check that it's working
set seed 09797
 myivreg .2 .5 .4 // .2 = X-Z correlation, .5 = X-E corr, 0.4 = Z-E corr.
 return list
 corr x e z
 
 
* now we make one monte carlo w/ 1000 replications, saving the following
* outputs:
global outputs "b=r(b) se=r(se) rmse=r(rmse) xzcorr=r(xzcorr) xecorr=r(xecorr) zecorr=r(zecorr)"
 
simulate $outputs, reps(1000):  myivreg .7 .5 .0
su
 
 
 
 
 
 timer on 1
 forv i = 1(1)7	{
	forv z = 1(1)7	{
 	local xzcorr = `i'/10
 	local xecorr = `z'/10
 simulate $outputs, reps(1000) saving(data`i'_`z',replace):  myivreg `xzcorr' `xecorr' .0 
	}
} 

use data1_1, clear
forv i = 1(1)7 {
	forv z = 1(1)7	{
		append using data`i'_`z'
	}
}
drop in 1/1000	// we have duplicates based off the above loop in 1_1
* 49,000 obs makes sense (7*7*1000)
timer off 1 
timer list 1
save ivresults, replace

use ivresults, clear
 
 
* plot some kdensities of our estimates of X
egen id = group(xzcorr xecorr)		// id these groups
 
twoway kdensity b if id == 34  || kdensity b if id== 49
* first is xz, xe corr == 0.5, second is corr == 0.7
 
 twoway kdensity rmse if id == 1 || kdensity rmse if id == 5 || kdensity rmse if id==8

 
* we can look at how beta changes and the se
preserve
collapse (mean) b se rmse xzcorr xecorr, by(id)
twoway connected b xzcorr if xecorr == float(.1) || connected b xzcorr if xecorr == .5, yline(2) xtitle("X-Z correlation (in decimals)")
more
twoway connected se id,   xtitle("X-Z correlation (in decimals)")
more 
twoway connected rmse id,   xtitle("X-Z correlation (in decimals)")
restore



* but the above doesn't really take into account the 2 dimensions we're 
* dealing with. A 3-d representation will: 
findit hmap
findit surface
preserve 
collapse (mean) b se rmse xzcorr xecorr, by(id)
hmap xzcorr xecorr rmse, monochrome
more 
surface xzcorr xecorr rmse
restore

* The surface plot seems more informative. here is another where we look
* at root mean sq deviations from "true" beta=2
gen dev_truth = sqrt((b-2)^2)
preserve 
collapse (mean) b se rmse xzcorr xecorr dev_truth, by(id)
surface xzcorr xecorr dev_truth
restore






