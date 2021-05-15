*
*	Andrew Q. Philips
*	8/30/16
*
*	Meta-analysis, first processing file (see README for details)
*
*	---------------------------------------------------------------------------

* NOTE: MAY NEED TO DOWNLOAD USING ``FINDIT BURD''
set scheme burd, perm


use meta_analysis_data.dta, clear


* FIGURE 1

* funnel plot: expenditures
twoway scatter parcorr parcorr_precision if dv_expenditures == 1, msym(x)  mcolor(green) xtitle("Precision (1/s.e.)") ytitle("Partial Correlation {&epsilon}{subscript:ij}") jitter(1) legend(off) title("Expenditures") yline(0)
graph save g1.gph, replace
* funnel plot: revenues
twoway  scatter parcorr parcorr_precision if dv_revenues == 1, msym(t)  mcolor(blue)  xtitle("Precision (1/s.e.)") ytitle("Partial Correlation {&epsilon}{subscript:ij}") jitter(1) legend(off) title("Revenues") yline(0)
graph save g2.gph, replace
* funnel plot: fiscal surplus
twoway scatter parcorr parcorr_precision if dv_fiscalbalance == 1, yline(0) msym(o) mcolor(black) xtitle("Precision (1/s.e.)") ytitle("Partial Correlation {&epsilon}{subscript:ij}") jitter(1) legend( off) title("Fiscal Balance")
graph save g3.gph, replace
* funnel plot: debt
twoway scatter parcorr parcorr_precision if dv_debt == 1, msym(plus) mcolor(red)  xtitle("Precision (1/s.e.)") ytitle("Partial Correlation {&epsilon}{subscript:ij}") jitter(1) legend( off) title("Debt") yline(0)
graph save g4.gph, replace

graph combine g1.gph g2.gph g3.gph g4.gph, rows(2) ycommon xcommon


* Get summary statistics for TABLE 1
preserve
drop if dv_marker == "dv_debt" // can't use debt for MRA
foreach var of varlist parcorr_se dv_fiscalbalance dv_debt dv_expenditures dv_revenues dv_rev_taxrevenue dv_rev_totrevenue dv_rev_otherrevenues dv_exp_totexpenditures dv_exp_grantstransfers dv_exp_capexpenditures dv_exp_curexpenditures dv_exp_administrative dv_exp_healtheducation dv_exp_other region_oecd region_latam region_asia region_subsaharan region_soviet avg_yr_in_sample temp_agg_quarterly temp_agg_monthly singlecountry muni_elec state_elec control_democracy control_coalition control_debt control_deficit control_GDP_any control_GDPgrowth control_govtexpenditure control_ideology control_inflation control_presidential control_proportional control_revenues control_transfers control_unemployment control_winmargin method_feunit method_dynamics method_olspcsegls elec_dummy elec_Franzese elec_half elec_exogenous elec_endogenous control_postelec control_prelec totmodels cites_peryr JournalImpactFactor {
	su `var'
	local aver = r(mean)
	*format `aver' %9.3f
	local stddev = r(sd)
	*format `stddev' %9.3f
	di ""
	di  "`var'"
	di  %3.2f `aver'
	di %3.2f `stddev'
}
restore




