*	Andy Philips
*   Texas A&M Univ.
*	Last Updated: 4/13/14
*	
*	--------------------------------------------------------------------------

/* Monte Carlos and Unit root tests	*/
* Set your directory:
cd "/Users/andyphilips/Documents/Methods/Monte Carlo"

/* In this Monte Carlo we will explore some properties of panel unit root
	tests. 
	
	The following program 
	allows us to create any number of panels that can be as long as we'd like.
	We can set the number of unit root series along with the level of
	autocorrelation in the stationary series...since this likely will affect
	the tests too.
*/

set seed 235943

capture program drop panelroots
program panelroots, rclass
version 12						
	drop _all
	args N T uroots	phi		 
	set obs `N'			// set total observations
	gen i = _n
	gen y = rnormal()	// generate our first y value for each series
	expand `T'			// expand out in time
	sort i
	by i: gen time = _n	// generate a time variable for each panel
	gen u = rnormal()
	gen x = -3+(3+3)*runiform()	// create uniform x between -3 and 3
	
	by i: replace y = `phi'*y[_n-1]+ 2*x + u if _n > 1 /*	first make all
	the panels stationary */
	forv m = 1/`uroots' {
		replace y = 1*y[_n-1]+ 2*x + u if _n > 1 & `m' == i
		label define i `m' "non-stationary", modify
	}	// this creates the non-stationary panels
	xtset i time		// set our data to stata's XT setting.
	
	*	Below are the various panel unit root tests:
	qui xtunitroot llc y						// levin lin chu
	return scalar llc_pval = r(p_tds)
	qui xtunitroot ips y						// im pesaran shin
	return scalar ips_pval = r(p_zttildebar)
	qui xtunitroot fisher y, dfuller lags(1)	// fisher-type
	return scalar fisher_pval =  r(p_Z)
	qui xtunitroot ht y							// harris tzavalis
	return scalar ht_pval =  r(p)
	qui xtunitroot breitung y					// breitung
	return scalar breitung_pval =  r(p)
	*qui xtunitroot hadri y						// hadri LM
	*return scalar hadri_pval =  r(p)
	/*	we won't do hadri since it has Ho that all panels are trend 
	stationary	*/
	
	scalar N = `N'		// keep our values for the table later:
	return scalar N = `N'
	scalar T = `T'
	return scalar T = `T'
	scalar phi = `phi'
	return scalar phi = `phi'
	scalar uroots = `uroots'
	return scalar uroots = `uroots'
end


* always test that our program is working:
panelroots 7 20 1 .2
return list
xtunitroot llc y 
xtline y, overlay 


* make a macro of our values we want to simulate:
global outputs  "  breitung = r(breitung_pval) ht= r(ht_pval) fisher = r(fisher_pval) ips=  r(ips_pval) llc =  r(llc_pval) "


*	Double check our simulate command works ok:
simulate $outputs N=r(N) T=r(T) phi=r(phi) uroots=r(uroots), reps(1000): ///
	panelroots 25 30 10 0.5
* in this we simulate 25 panels of length 30 with 10 unit roots. Stationary
* processes are phi=.5




*	In this one, we are varying the length of the series, number of unit roots
*	and the rho value. We hold the # in the series constant at 25 
*   i is our rho value
set seed 34665
timer on 1			// nice to see how long our program takes to run.
forv i=1(2)7	{
	* l is the length of our series
	forv l = 10(10)20	{
		* u is the number of panels w/ unit roots
		forv u = 0(10)20 {
			local rho = `i'/10	// get rho from .1 -> .9 by .2
simulate $outputs N=r(N) T=r(T) phi=r(phi) uroots=r(uroots), reps(1000) nodots saving(data`i'_`l'_`u',replace): panelroots 25 `l' `u' `i'
		}
	}	
}

timer off 1
timer list 1		// display how long this took to run

* now grab these datasets, append them, and view it
use "data1_10_0", clear
forv  i=1(2)7	{
	forv l = 10(10)20	{
		forv u = 0(10)20	{
append using "data`i'_`l'_`u'"
		}
	}
}



save "unitroot results.dta", replace
* Because this monte carlo takes a long time to run, we 
* can open up a dataset that already contains the results
*use "unitroot results.dta", clear


egen id = group(N T phi uroots)
tabstat breitung-uroots , stat(mean) by(id )
/* we may not always get a result for the IPS test...according to Stata: "For
 the asymptotic normal distribution of Z_t-tilde-bar to hold, T must be at 
 least 5 if the dataset is strongly balanced and the deterministic part of 
 the model includes only panel-specific means, or at least 6 if time trends 
 are also included.  If the data are not strongly balanced, then T must be at
  least 9 for the asymptotic  distribution to hold.  If these limits on T 
  are not met, the p-value for Z_t-tilde-bar is not reported." I think this
  is why it is missing. */
  
  
* here's a k-density of the Fisher test p-value across phi
twoway kdensity fisher if uroots == 10  & N==25 & T == 20 & phi == 1 || ///
 kdensity fisher if uroots == 10 & N==25 & T == 20 & phi == 3 || ///
  kdensity fisher if uroots == 10 & N==25 & T == 20 & phi == 5 || ///
   kdensity fisher if uroots == 10 & N==25 & T == 20 & phi == 7, ///
    legend( order(1 "phi = .1" 2 "phi = .3" 3 "phi = .5" 4 "phi = .7" )) ///
     xtitle("value of the test") xline(0.05)

* we can collapse our data so that we can look at various changes in performance
* as we change values
collapse (mean) breitung-uroots, by(id)
* for example, we can create graphs such as this, which varies phi values
twoway connected breitung uroots if phi==1 & T==20 || connected breitung uroots ///
if phi==3 & T==20 || connected breitung uroots if phi==5 & T==20 || ///
connected breitung uroots if phi==7 & T==10, ///
 legend( order(1 "phi = .1" 2 "phi = .3" 3 "phi = .5" 4 "phi = .7"))


