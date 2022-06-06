* Article: A Moveable Benefit? Spillover Effects of Quotas on Women's Numerical Representation. Forthcoming at PRQ
* Authors: Komal Preet Kaur, Andrew Q. Philips
* Date 6/5/22
* NOTE 1: "scheme-burd" used for analysis, so graphics scheme might look different. For same graphics scheme, findit search for scheme-burd and set new theme.
* NOTE 2: the pvar suite of commands (pvar pvarsoc, pvarstable and pvargranger), coefplot, and twowayfeweights are user-written commands and must be downloaded/installed before running these commands.
* ------------------------------------------------------------------------

* open the combined datset:
use "combineddata.dta", clear
xtset pc01_state_id election_counter
set seed 9450900 // set seed b/c of bootstrap reps

* ---------------------------------------------------------------
*	PART I: PANEL VAR
* ---------------------------------------------------------------
pvarsoc pcwom probcon votesharewom wonwomoverall probconwon, exog(treat50_post) maxlag(1) pinstl(1/2) // N*T = 92, MBIC = -96.31267

* ---------- TABLE S11: PVAR RESULTS -------------------------------------
pvar pcwom probcon votesharewom wonwomoverall probconwon, exog(treat50_post l.treat50_post) instlags(1/2) overid fod vce(cluster pc01_state_id) gmmstyle td // pass Hansen's J (chi2 = 5.3373022).  GMM Criterion Q(b) = .0376
* ------------------------------------------------------------------------

* ---------- FIGURE S6: PVAR STABILITY -----------------------------------
pvarstable , graph // stable
*graph export "pvar-stable.pdf", as(pdf) replace
* ------------------------------------------------------------------------

* ---------- TABLE S12 AND TABLE 4: GRANGER TESTS ------------------------
pvargranger 
* ------------------------------------------------------------------------

* create IRFs for plotting:
foreach var of varlist pcwom probcon votesharewom wonwomoverall probconwon {
	egen z`var' = std(`var') // standardize so shocks are all + 1 SD
}
pvar zpcwom zprobcon zvotesharewom zwonwomoverall zprobconwon, exog(treat50_post l.treat50_post) instlags(1/2) overid fod gmmstyle td 
pvarirf, mc(1000) step(4) level(95) save("temp1", replace) nodraw
pvarirf, mc(1000) step(4) level(90) save("temp2", replace) nodraw

preserve // NOTE: need to run this all the way to 'restore'
use temp1.dta, clear
rename ll ll_95
rename ul ul_95
joinby _step impres using "temp2.dta", unmatched(both)
drop _merge
rename ll ll_90
rename ul ul_90
decode impres, generate(shock)

* ---------- FIGURE S7 ------------------------
* shock of zpcwom 
twoway rarea ll_95 ul_95 _step if shock == "zpcwom : zpcwom", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zpcwom : zpcwom", color(blue%70) || line irf _step if shock == "zpcwom : zpcwom", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Women candidates", size(medsmall))
graph save g1.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zpcwom : zprobcon", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zpcwom : zprobcon", color(blue%70) || line irf _step if shock == "zpcwom : zprobcon", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("%  Races with at least one woman", size(medsmall))
graph save g2.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zpcwom : zvotesharewom", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zpcwom : zvotesharewom", color(blue%70) || line irf _step if shock == "zpcwom : zvotesharewom", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("Vote share", size(medsmall))
graph save g3.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zpcwom : zwonwomoverall", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zpcwom : zwonwomoverall", color(blue%70) || line irf _step if shock == "zpcwom : zwonwomoverall", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Women winners", size(medsmall))
graph save g4.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zpcwom : zprobconwon", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zpcwom : zprobconwon", color(blue%70) || line irf _step if shock == "zpcwom : zprobconwon", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Races with women winners", size(medsmall))
graph save g5.gph, replace
graph combine g1.gph g2.gph g3.gph g4.gph g5.gph, ysize(5) rows(3) ycommon
*graph export "pvar-IRF-pcwom.pdf", as(pdf) replace
* ---------------------------------------------------------------

* ----------------- FIGURE S8 ------------------------------------------
* shock of zprobcon 
twoway rarea ll_95 ul_95 _step if shock == "zprobcon : zpcwom", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zprobcon : zpcwom", color(blue%70) || line irf _step if shock == "zprobcon : zpcwom", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Women candidates", size(medsmall))
graph save g1.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zprobcon : zprobcon", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zprobcon : zprobcon", color(blue%70) || line irf _step if shock == "zprobcon : zprobcon", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Races with at least one woman", size(medsmall))
graph save g2.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zprobcon : zvotesharewom", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zprobcon : zvotesharewom", color(blue%70) || line irf _step if shock == "zprobcon : zvotesharewom", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("Vote share", size(medsmall))
graph save g3.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zprobcon : zwonwomoverall", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zprobcon : zwonwomoverall", color(blue%70) || line irf _step if shock == "zprobcon : zwonwomoverall", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Women winners", size(medsmall))
graph save g4.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zprobcon : zprobconwon", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zprobcon : zprobconwon", color(blue%70) || line irf _step if shock == "zprobcon : zprobconwon", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Races with women winners", size(medsmall))
graph save g5.gph, replace
graph combine g1.gph g2.gph g3.gph g4.gph g5.gph, ysize(5) rows(3) ycommon
*graph export "pvar-IRF-probcon.pdf", as(pdf) replace
* ---------------------------------------------------------------

* ----------------- FIGURE S9 ------------------------------------------
* shock of zvotesharewom 
twoway rarea ll_95 ul_95 _step if shock == "zvotesharewom : zpcwom", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zvotesharewom : zpcwom", color(blue%70) || line irf _step if shock == "zvotesharewom : zpcwom", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Women candidates", size(medsmall))
graph save g1.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zvotesharewom : zprobcon", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zvotesharewom : zprobcon", color(blue%70) || line irf _step if shock == "zvotesharewom : zprobcon", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Races with at least one woman", size(medsmall))
graph save g2.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zvotesharewom : zvotesharewom", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zvotesharewom : zvotesharewom", color(blue%70) || line irf _step if shock == "zvotesharewom : zvotesharewom", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("Vote share", size(medsmall))
graph save g3.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zvotesharewom : zwonwomoverall", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zvotesharewom : zwonwomoverall", color(blue%70) || line irf _step if shock == "zvotesharewom : zwonwomoverall", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Women winners", size(medsmall))
graph save g4.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zvotesharewom : zprobconwon", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zvotesharewom : zprobconwon", color(blue%70) || line irf _step if shock == "zvotesharewom : zprobconwon", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Races with women winners", size(medsmall))
graph save g5.gph, replace
graph combine g1.gph g2.gph g3.gph g4.gph g5.gph, ysize(5) rows(3) ycommon
*graph export "pvar-IRF-votesharewom.pdf", as(pdf) replace
* ---------------------------------------------------------------

* ----------------- FIGURE 2 ------------------------------------------
* shock of zwonwomoverall 
twoway rarea ll_95 ul_95 _step if shock == "zwonwomoverall : zpcwom", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zwonwomoverall : zpcwom", color(blue%70) || line irf _step if shock == "zwonwomoverall : zpcwom", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Women candidates", size(medsmall))
graph save g1.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zwonwomoverall : zprobcon", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zwonwomoverall : zprobcon", color(blue%70) || line irf _step if shock == "zwonwomoverall : zprobcon", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Races with at least one woman", size(medsmall))
graph save g2.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zwonwomoverall : zvotesharewom", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zwonwomoverall : zvotesharewom", color(blue%70) || line irf _step if shock == "zwonwomoverall : zvotesharewom", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("Vote share", size(medsmall))
graph save g3.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zwonwomoverall : zwonwomoverall", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zwonwomoverall : zwonwomoverall", color(blue%70) || line irf _step if shock == "zwonwomoverall : zwonwomoverall", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Women winners", size(medsmall))
graph save g4.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zwonwomoverall : zprobconwon", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zwonwomoverall : zprobconwon", color(blue%70) || line irf _step if shock == "zwonwomoverall : zprobconwon", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Races with women winners", size(medsmall))
graph save g5.gph, replace
graph combine g1.gph g2.gph g3.gph g4.gph g5.gph, ysize(5) rows(3) ycommon
*graph export "pvar-IRF-wonwomoverall.pdf", as(pdf) replace
* ---------------------------------------------------------------

* ----------------- FIGURE 3 ------------------------------------------
* shock of zprobconwon 
twoway rarea ll_95 ul_95 _step if shock == "zprobconwon : zpcwom", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zprobconwon : zpcwom", color(blue%70) || line irf _step if shock == "zprobconwon : zpcwom", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Women candidates", size(medsmall))
graph save g1.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zprobconwon : zprobcon", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zprobconwon : zprobcon", color(blue%70) || line irf _step if shock == "zprobconwon : zprobcon", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Races with at least one woman", size(medsmall))
graph save g2.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zprobconwon : zvotesharewom", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zprobconwon : zvotesharewom", color(blue%70) || line irf _step if shock == "zprobconwon : zvotesharewom", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("Vote share", size(medsmall))
graph save g3.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zprobconwon : zwonwomoverall", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zprobconwon : zwonwomoverall", color(blue%70) || line irf _step if shock == "zprobconwon : zwonwomoverall", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Women winners", size(medsmall))
graph save g4.gph, replace
twoway rarea ll_95 ul_95 _step if shock == "zprobconwon : zprobconwon", color(blue%40)  || rarea ll_90 ul_90 _step if shock == "zprobconwon : zprobconwon", color(blue%70) || line irf _step if shock == "zprobconwon : zprobconwon", lwidth(medthick) yline(0) lcolor(black) legend(off) xtitle("Election") ytitle("Response") title("% Races with women winners", size(medsmall))
graph save g5.gph, replace
graph combine g1.gph g2.gph g3.gph g4.gph g5.gph, ysize(5) rows(3) ycommon
*graph export "pvar-IRF-probconwon.pdf", as(pdf) replace
* ---------------------------------------------------------------

restore
* ---------------------------------------------------------------
* ---------------------------------------------------------------


* ---------------------------------------------------------------
*	PART II: PARALLEL TRENDS (AND POST-TREATMENT DYNAMIC EFFECTS)
* ---------------------------------------------------------------
* It's easy to test parallel trends when the treatment period occurs at the same time (for not all units of course). Harder when we want to `standardize' time (i.e., have it as ..., -2, -1, 0 [treatment time], 1, 2,...). This is because there's no way to standardize units that were never treated. An alternative is something done by Stevenson and Wolfers (2006, see also Cerulli and Ventura 2019, and Cunningham). Let D_{it} = 1 for all i treated in the first observed treatment period, D_{i,t + 1} = 1 for the next period,... and D_{i,t-1} for the period just before treatment, etc...center s.t. treatment D_{i,t} is the omitted category and plot all coefs
* first create variable = 1 in treatment period only:
xtset 
sort pc01_state_name year
bysort pc01_state_name: gen treatcum = sum(treat50_post)
gen t_plus1 = 0
replace t_plus1 = 1 if treatcum == 2
replace treatcum = 0 if treatcum > 1
* let's create pre-post variables
xtset
forv i = 1/9 {
	gen t_minus`i' = f`i'.treatcum // pre-treatment is forward
	replace t_minus`i' = 0 if t_minus`i' == .
}
* label
forv i = 1/9 {
	lab var t_minus`i' "t-`i'"
}
lab var treatcum  "t"
lab var t_plus1 "t+1"

* ---------------------- FIGURE S1 ----------------------------------------
xtreg pcwom t_minus9 t_minus8 t_minus7 t_minus6 t_minus5 t_minus4 t_minus3 t_minus2 t_minus1 t_plus1 i.election_counter, fe vce(cluster pc01_state_id)

coefplot, vertical drop(_cons 1b.election_counter 2.election_counter 3.election_counter 4.election_counter 5.election_counter 6.election_counter 7.election_counter 8.election_counter 9.election_counter 10.election_counter) yline(0) xtitle(Time Relative to Treatment) msymbol(O) mcolor(black) ///
 levels(95 90 80 70) ciopts(lwidth(3 ..) lcolor(*.4 *.6 *.8 *1)) ///
    legend(order(1 "95" 2 "90" 3 "80" 4 "70") rows(1)) title("% Women Candidates") xline(9.5)
*graph export "pttest1_pcwom.pdf", as(pdf) replace
* ------------------------------------------------------------------------

* ---------------------- FIGURE S2 ----------------------------------------
xtreg probcon t_minus9 t_minus8 t_minus7 t_minus6 t_minus5 t_minus4 t_minus3 t_minus2 t_minus1 t_plus1 i.election_counter, fe vce(cluster pc01_state_id)

coefplot, vertical drop(_cons 1b.election_counter 2.election_counter 3.election_counter 4.election_counter 5.election_counter 6.election_counter 7.election_counter 8.election_counter 9.election_counter 10.election_counter) yline(0) xtitle(Time Relative to Treatment) msymbol(O) mcolor(black) ///
 levels(95 90 80 70) ciopts(lwidth(3 ..) lcolor(*.4 *.6 *.8 *1)) ///
    legend(order(1 "95" 2 "90" 3 "80" 4 "70") rows(1)) title("% of Races with at Least One Woman Candidate") xline(9.5)
*graph export "pttest1_probcon.pdf", as(pdf) replace
* ------------------------------------------------------------------------

* ---------------------- FIGURE S3 ----------------------------------------
xtreg votesharewom t_minus9 t_minus8 t_minus7 t_minus6 t_minus5 t_minus4 t_minus3 t_minus2 t_minus1 t_plus1 i.election_counter, fe  vce(cluster pc01_state_id)

coefplot, vertical drop(_cons 1b.election_counter 2.election_counter 3.election_counter 4.election_counter 5.election_counter 6.election_counter 7.election_counter 8.election_counter 9.election_counter 10.election_counter) yline(0) xtitle(Time Relative to Treatment) msymbol(O) mcolor(black) ///
 levels(95 90 80 70) ciopts(lwidth(3 ..) lcolor(*.4 *.6 *.8 *1)) ///
    legend(order(1 "95" 2 "90" 3 "80" 4 "70") rows(1)) title("Average Vote Share of Women") xline(9.5)
*graph export "pttest1_votesharewom.pdf", as(pdf) replace
* ------------------------------------------------------------------------

* ---------------------- FIGURE S4 ----------------------------------------
xtreg wonwomoverall t_minus9 t_minus8 t_minus7 t_minus6 t_minus5 t_minus4 t_minus3 t_minus2 t_minus1 t_plus1 i.election_counter, fe vce(cluster pc01_state_id)

coefplot, vertical drop(_cons 1b.election_counter 2.election_counter 3.election_counter 4.election_counter 5.election_counter 6.election_counter 7.election_counter 8.election_counter 9.election_counter 10.election_counter) yline(0) xtitle(Time Relative to Treatment) msymbol(O) mcolor(black) ///
 levels(95 90 80 70) ciopts(lwidth(3 ..) lcolor(*.4 *.6 *.8 *1)) ///
    legend(order(1 "95" 2 "90" 3 "80" 4 "70") rows(1)) title("% Women winners") xline(9.5)
*graph export "pttest1_wonwomoverall.pdf", as(pdf) replace
* ------------------------------------------------------------------------

* ---------------------- FIGURE S5 ----------------------------------------
xtreg probconwon t_minus9 t_minus8 t_minus7 t_minus6 t_minus5 t_minus4 t_minus3 t_minus2 t_minus1 t_plus1 i.election_counter, fe vce(cluster pc01_state_id)

coefplot, vertical drop(_cons 1b.election_counter 2.election_counter 3.election_counter 4.election_counter 5.election_counter 6.election_counter 7.election_counter 8.election_counter 9.election_counter 10.election_counter) yline(0) xtitle(Time Relative to Treatment) msymbol(O) mcolor(black) ///
 levels(95 90 80 70) ciopts(lwidth(3 ..) lcolor(*.4 *.6 *.8 *1)) ///
    legend(order(1 "95" 2 "90" 3 "80" 4 "70") rows(1)) title("% Races Where a Woman Won") xline(9.5)
*graph export "pttest1_probconwon.pdf", as(pdf) replace
* ------------------------------------------------------------------------


* ---------------------- TABLE S2 ------------------------------------
* post-treatment effects:
xtreg pcwom treatcum t_plus1 i.election_counter, fe vce(cluster pc01_state_id)
xtreg pcwom treatcum t_plus1 i.election_counter c.election_counter##i.treatedstate, fe vce(cluster pc01_state_id)

xtreg probcon treatcum t_plus1 i.election_counter, fe vce(cluster pc01_state_id)
xtreg probcon treatcum t_plus1 i.election_counter c.election_counter##i.treatedstate, fe vce(cluster pc01_state_id)
* ------------------------------------------------------------------------


* ---------------------- TABLE S3 ------------------------------------
xtreg votesharewom treatcum t_plus1 i.election_counter, fe vce(cluster pc01_state_id) 
xtreg votesharewom treatcum t_plus1 i.election_counter c.election_counter##i.treatedstate, fe vce(cluster pc01_state_id)

xtreg wonwomoverall treatcum t_plus1 i.election_counter, fe vce(cluster pc01_state_id)
xtreg wonwomoverall treatcum t_plus1 i.election_counter c.election_counter##i.treatedstate, fe vce(cluster pc01_state_id)

xtreg probconwon treatcum t_plus1 i.election_counter, fe vce(cluster pc01_state_id)
xtreg probconwon treatcum t_plus1 i.election_counter c.election_counter##i.treatedstate, fe vce(cluster pc01_state_id)
* ------------------------------------------------------------------------


* ---------------------------------------------------------------
* ---------------------------------------------------------------



* ---------------------------------------------------------------
* ---------------------------------------------------------------

* ---------------------------------------------------------------
*	PART III: WEIGHTED/ROBUST DID
* ---------------------------------------------------------------

* ---------------------- TABLE S1 ------------------------------------
set seed 9450900
* Fraction of weights, under assumption of common trends. beta comes from a weighted sum of 25 ATTs. 
twowayfeweights pcwom pc01_state_id election_counter treat50_post, type(feTR) // percent of women contesting elections
* 0/25 get negative weight
twowayfeweights probcon pc01_state_id election_counter treat50_post, type(feTR) // % of races with at least 1 woman candidate
* 0/25 get negative weight
twowayfeweights votesharewom pc01_state_id election_counter treat50_post, type(feTR) // average vote share of women
* 0/25 get negative weight
twowayfeweights wonwomoverall pc01_state_id election_counter treat50_post, type(feTR) // the % of women winners from total candidates
* 0/25 get negative weight
twowayfeweights probconwon pc01_state_id election_counter treat50_post, type(feTR) // % races where a woman won
* 0/25 get negative weight

* fraction of weights, under common trends, treatment monotinicity, and if group's treatment effect does not change over time, beta estimates weighted sum of 17 LATEs:
twowayfeweights pcwom pc01_state_id election_counter treat50_post, type(feS) // percent of women contesting elections
* 0/17 get negative weight
twowayfeweights probcon pc01_state_id election_counter treat50_post, type(feS) // % of races with at least 1 woman candidate
* 0/17 get negative weight
twowayfeweights votesharewom pc01_state_id election_counter treat50_post, type(feS) // average vote share of women
* 0/17 get negative weight
twowayfeweights wonwomoverall pc01_state_id election_counter treat50_post, type(feS) // the % of women winners from total candidates
* 0/17 get negative weight
twowayfeweights probconwon pc01_state_id election_counter treat50_post, type(feS) // % races where a woman won
* 0/17 get negative weight
* ---------------------------------------------------------------
* ---------------------------------------------------------------
