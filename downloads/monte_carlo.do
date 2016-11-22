* Andy Philips
* Texas A&M
* 7/27/14
* Monte Carlo Simulation -----------------------------------------------------
/*	This .do file examines Monte Carlo Simulations using two examples */

* Set your directory:
cd "/Users/andyphilips/Documents/Methods/Monte Carlo"

* MCs simulate finite data from a hypothesized model...usually we want to 
* examine our confidence intervals to verify that they are the `true' intervals

*	------------------------------------------------------------------------
/* This first example comes straight from Cameron and Trivedi. It is on the 
Central Limit Theorem in action: our sample mean will converge on the 
population mean as N -> infinity and approximate a normal distribution. */

clear
set seed 23443 	// so we can replicate this in the future
set obs 30		// drawing 30 observations from 1 sample
generate x = runiform()	//	generate x from a uniform dist.
summ x

hist x	, bin(10)	// lets look at a histogram and k-density plot of x
kdensity x

/* so not a perfect uniform distribution...but definitely NOT a normal
distribution. What happens when we repeat sampling x over thousands of
simulations?  The mean of a (0,1) uniform is 0.5, the standard deviation 
will be sqrt(1/12), and the standard deviation of the sample-mean estimator 
is sqrt((1/12/)30) since we have 30 obs.  */

/* before we perform a Monte Carlo, we need to design a program that tells 
Stata what to do (esentially what we did above). 	*/

capture program drop clt	
program define clt, rclass			// define the program
version 12							// make Stata use Stata12 settings
	drop _all						// drop anything else in memory
	quietly set obs 30				// 30 obs (quietly= don't display output)
	generate x = runiform()			// create (0,1) uniform distribution
	summ	x						
	return scalar samplemean = r(mean)	// returns a scalar (=number) mean
end


* double check our program works:
clt
return list


/*	Here is the Simulate command which runs the Monte Carlo. we simulate
xbar, is the mean of each sample estimator. Seed allows us to replicate the
pseudo-random numbers we generated. reps gives the # of replications.	*/ 
simulate xbar = r(samplemean), seed(2345) reps(10000): clt

* lots of dots pop up. these show that each of the 10000 replications has 
* taken place. (you can suppress them with the nodots option)

su xbar
* Mean is nearly the population mean of 0.5
disp sqrt((1/12)/30)
* and the std. dev. is very close to the population std. dev.
hist xbar, normal title("CLT at Work")



*	------------------------------------------------------------------------
/*	This second example concerns Autoregressive processes */

clear
set seed 800087
set obs 120						// 120 observations
egen time = fill(0 1 2)			
tsset time						// generate a time variable and set it

generate epsilon = rnormal()	// normal error
generate y = rnormal()			// generate a normally dist.
generate x = runiform()*3
scalar rho = 0.6				// AR parameter
* now create the AR process. rho = 0.6, and x has a true parameter value of 2
* we also include a constant of 1.
replace y = 1 + 2*x + rho*l.y + epsilon if time >0	
drop if time == 0
tsline y, yline(0)

/* how does a regression of an AR process compare to a process without one?
	lets compare the two.  First, a static OLS regression where we do not
	take into account the AR process	*/
	
reg y x
predict res1, residuals
twoway scatter time res1, yline(0) 
* let's fit a line into this:
graph twoway (lfitci time res1) (scatter time res1), yline(0)
* even worse is examinining the relationship between y and the residuals:
twoway scatter y res1
* Yikes! We can use all of Stata's AR diagnostic statistics to see this as well
estat archlm
estat bgodfrey
estat durbinalt
estat dwatson		// remember that 2 = no autocorrelation

/* What makes this better? One approach would be to use a GLS method
such as the Prais Winsten transformation to account for serial correlation */
prais y x
predict res2, resid
twoway scatter y res2

* Note that we could always model the process with a lagged DV (as long as 
*  we were sure this was an AR process and not a unit root...ie. rho=1
reg y l.y x
predict res3, resid
twoway scatter y res3

* let's see fit lines for all the resids
graph twoway (scatter y res1) (scatter y res2) (scatter y res3), legend( order(1 "OLS" 2 "Prais" 3 "OLS w LDV" ))
graph twoway (lfit y res1) (lfit y res2) (lfit y res3), legend( order(1 "OLS" 2 "Prais" 3 "OLS w LDV" ))
* actually, prais does not help here all that much, our residuals may 
* have the right "slope" but they are still being over/under-fit
* as the high constant in the regression indicates

/* we can use Monte Carlos to examine the conditions (of rho) under which
 each may be preferred. How high must the AR rho be before we reject the 
 null of the parameter on X, H_0: b1 = 2 ? In effect, what are the finite
 sample properties of OLS? */



capture program drop ARsim
program ARsim, rclass
version 12						
	drop _all
	args obs rho alphaval			//	allows us to set our observations and rho 
	set obs `obs' 			// actually set the observations
	scalar rho = `rho'		// actually set rho
	return scalar rho = `rho'
	scalar alphaval = `alphaval'		// set alpha for rejecting b1=2
	egen time = fill(0 1 2)			
	tsset time						// generate a time variable and set it
	generate epsilon = rnormal()	// normal error
	generate y = rnormal()			// generate a normally dist. y
	generate x = -3+(3+3)*runiform()	// create uniform x between -3 and 3
* now create the AR process. x has a true parameter value of 2
* we also include a constant of 1.
	replace y = 1 + 2*x + rho*l.y + epsilon if time >0	
	drop if time == 0

*	Model 1: OLS
	qui regress y x						
	return scalar b_ols = _b[x]			// grab our betas and std. errors
	return scalar se_ols = _se[x]
	* getting upper and lower ci's of b1:
	* return scalar lb_ols = _b[x] -_se[x]*invttail(e(df_r),alphaval/2)
	* return scalar ub_ols = _b[x] +_se[x]*invttail(e(df_r),alphaval/2)
	* get p-value of b1 NOT being equal to its true value of 2
	* i.e. test = (b1 - 2)/se_b1
	return scalar test_ols = (_b[x]-2)/_se[x]
	return scalar pv_ols = 2*ttail(e(df_r),abs(return(test_ols)))
	* dummy variable = 1 if we can reject H_0: b1 = 2
	return scalar reject_ols =	///
	 	abs(return(test_ols))>invttail(e(df_r),alphaval/2)
	* last, lets get a RMSE measure
	return scalar rmse_ols = e(rmse)
	 	
*	Model 2: Prais
	qui prais y x						
	return scalar b_prais = _b[x]			// grab our betas and std. errors
	return scalar se_prais = _se[x]
	* getting upper and lower ci's of b1:
	* return scalar lb_prais = _b[x] -_se[x]*invttail(e(df_r),alphaval/2)
	* return scalar ub_prais = _b[x] +_se[x]*invttail(e(df_r),alphaval/2)
	* get p-value of b1 NOT being equal to its true value of 2
	* i.e. test = (b1 - 2)/se_b1
	return scalar test_prais = (_b[x]-2)/_se[x]
	return scalar pv_prais = 2*ttail(e(df_r),abs(return(test_prais)))
	* dummy variable = 1 if we can reject H_0: b1 = 2
	return scalar reject_prais = ///
		abs(return(test_prais))>invttail(e(df_r),alphaval/2)
	* now we have an estimate of rho
	return scalar rho_prais = e(rho)
	return scalar rmse_prais = e(rmse)
	
*	Model 3: OLS w LDV
	qui reg y l.y x						
	return scalar b_ldv = _b[x]			// grab our betas and std. errors
	return scalar se_ldv = _se[x]
	* getting upper and lower ci's of b1:
	* return scalar lb_ldv = _b[x] -_se[x]*invttail(e(df_r),alphaval/2)
	* return scalar ub_ldv = _b[x] +_se[x]*invttail(e(df_r),alphaval/2)
	* get p-value of b1 NOT being equal to its true value of 2
	* i.e. test = (b1 - 2)/se_b1
	return scalar test_ldv = (_b[x]-2)/_se[x]
	return scalar pv_ldv = 2*ttail(e(df_r),abs(return(test_ldv)))
	* dummy variable = 1 if we can reject H_0: b1 = 2
	return scalar reject_ldv = ///
		abs(return(test_ldv))>invttail(e(df_r),alphaval/2)
	* now we have an estimate of rho: in the parameter on the ldv
	return scalar rho_ldv = _b[l.y]
	return scalar rmse_ldv = e(rmse)

end

* always check a sample just to make sure it's working
ARsim 30 0.90 0.05
return list
test x = 2

/* so while there is a lot going on above, the output is 
straighforward: we run OLS, Prais, OLS-LDV and grab the coefficient on x
to see if it is inconsistent. We have a t-test of if x=2 and a dummy variable
indicating if we can reject that x=2. we also get the RMSE of each model	*/


set seed 098766
/* here is the command. note we have 1000 simulations and are looking at
 the scalars of interest. after the ":" we put our program in, specifying
 30 observations, rho=.9, and alpha for rejection of 0.05 */
 
simulate b_ols=r(b_ols) se_ols=r(se_ols) pv_ols=r(pv_ols)		///
 reject_ols = r(reject_ols) rmse_ols = r(rmse_ols)				///
 b_prais=r(b_prais) se_prais=r(se_prais) pv_prais=r(pv_prais)	///
 reject_prais = r(reject_prais) rmse_prais = r(rmse_prais)		///
 b_ldv=r(b_ldv) se_ldv=r(se_ldv) pv_ldv=r(pv_ldv)				///
 reject_ldv = r(reject_ldv) rmse_ldv = r(rmse_ldv)				///
 , reps(1000) : ARsim 30 .90 .05

* now summarize the results:
su
/* note we only have 995 replications; sometimes a simulation will crash, as
evidenced by an x instead of a . in the output. Notice how OLS has the wildest
beta estimates and a large RMSE.  

Prais is more consistent but seems to be biased downwards; we can reject the 
null of beta=2 (the true parameter) 99 % of the time! OLS rejects about
12 % of the time, and OLS-LDV only 4.6%.
*/


* lets do the same for some other values of observations. to save time,
* we make a macro of what we want to simulate:
global outputs  "b_ols=r(b_ols) se_ols=r(se_ols) pv_ols=r(pv_ols) reject_ols = r(reject_ols) rmse_ols = r(rmse_ols) b_prais=r(b_prais) se_prais=r(se_prais) pv_prais=r(pv_prais) reject_prais = r(reject_prais) rmse_prais = r(rmse_prais) b_ldv=r(b_ldv) se_ldv=r(se_ldv) pv_ldv=r(pv_ldv) reject_ldv = r(reject_ldv) rmse_ldv = r(rmse_ldv)"

* we also will suppress the dots
simulate $outputs, reps(1000) nodots: ARsim 100 .90 0.05
su




* finally, we can get a nice-looking table of a variety of values that we 
* specify by running a loop and saving the observations
forv i=1/9	{
	local rho = `i'/10	// get rho from .1 -> 1 by .1
	simulate $outputs rho=r(rho), reps(100) nodots saving(rho`i',replace): ARsim 100 `rho' 0.05
}
* now grab these datasets, append them, and view it
use rho1
forv  i=2/9	{
	append using rho`i'
}
* first, we look at the estimates of 
tabstat b_* se_* , stat(mean) by(rho)

tabstat reject_* pv_*, stat(mean) by(rho)

tabstat rmse_*, stat(mean) by(rho)

/* Another way we can look at our results is with the user-written
 simsum command. */
findit simsum
 
/* b_* = specify our betas, true = true value of coefficient, se = our
standard error estimates, ref = reference method (OLS in our case,
and we change the format a bit. Keep in mind that since we have 
numerous rho values in this dataset we only want to look at 1 at a time:.
We can also ask for a graph using the option */

* for rho = .1
simsum b_* in 1/100, true(2) se(se_*) ref(b_ols) format(%7.0g) graph
* for rho = .5
simsum b_* in 401/500, true(2) se(se_*) ref(b_ols) format(%7.0g)
* for rho = .9
simsum b_* in 801/900, true(2) se(se_*) ref(b_ols) format(%7.0g)

/* it is clear that there are substantial gains to be made from Prais and 
the LDV model over OLS, as shown by the simsum command.  This is especially
true as rho gets large*/
