*
*	Andy Philips
*	8/30/16
*
*	Meta-analysis effect size plotting (see README for details)
*
*	---------------------------------------------------------------------------

* NOTE: MAY NEED TO DOWNLOAD USING ``FINDIT BURD''
set scheme burd, perm

* First the main analysis, which is weighted by the # of observations:
insheet using "output.csv", clear

gen id = _n
rename v1 name
rename v2 obs
rename v3 v1
rename v4 s1
rename v5 v2
rename v6 s2
rename v7 v3
rename v8 s3
rename v9 v4
rename v10 s4
reshape long v s, i(id) j(type)
gen var = ""
replace var = "UW" if type == 1
replace var = "RE" if type == 2
replace var = "FE" if type == 3
replace var = "HS" if type ==4
rename v beta
rename s se
gen ul = beta + invttail(obs,.025)*se
gen ll = beta - invttail(obs,.025)*se

gen name2 = ""
replace name2 =  "Debt" if name=="dv_debt"
replace name2 = "Administration" if name=="dv_exp_administrative"
replace name2 ="Capital Expenditures" if name=="dv_exp_capexpenditures"
replace name2 ="Current Expenditures" if name=="dv_exp_curexpenditures"
replace name2 ="Inter--Govt. Grants" if name=="dv_exp_grantstransfers"
replace name2 ="Health/Education" if name=="dv_exp_healtheducation"
replace name2 ="Other Expenditures" if name=="dv_exp_other"
replace name2 ="Total Expenditures" if name=="dv_exp_totexpenditures"
replace name2 ="Expenditures" if name=="dv_expenditures"
replace name2 ="Fiscal Surplus" if name=="dv_fiscalbalance"
replace name2 ="Other Revenues" if name=="dv_rev_otherrevenues"
replace name2 ="Tax Revenue" if name=="dv_rev_taxrevenue"
replace name2 ="Total Revenue" if name=="dv_rev_totrevenue"
replace name2 ="Revenues" if name=="dv_revenues"

* ---------------------------------------
* for big categories:
preserve
keep if inlist(id, 1, 2,3,4) 
sort id
replace id = 0 if id == 1
replace id = 1 if id == 4
replace id = 4 if id == 0

gen pad = id+(type/6)
gen junk = pad +.4
global texthere ""
levelsof id, local(levels)
foreach i of local levels	{
	
	if `i' == 1	{
		local tx`i' "Fiscal Surplus (234)"
	}
	if `i' == 2	{
		local tx`i' "Revenues (243)"
	}
	if `i' == 3	{
		local tx`i' "Debt (22)"
	}
	if `i' == 4	{
		local tx`i' "Expenditures (699)"
	}
	
	local xpos`i' = .12
	su pad if id == `i' & var == "FE", meanonly
	local ypos`i' = r(mean) 
	global texthere "$texthere text(`ypos`i'' `xpos`i'' "`tx`i''", size(medsmall) )"
}

twoway scatter pad beta if var == "UW", msymbol(T) mcolor(black) || ///
	scatter pad beta if var == "RE", msymbol(O) mcolor(black)	||	///
	scatter pad beta if var == "FE", msymbol(X) mcolor(black) || 	///
	scatter pad beta if var == "HS", msymbol(S) mcolor(black)	||	///
	rspike ul ll pad if var == "UW", horizontal lcolor(black) ||	///
	rspike ul ll pad if var == "RE", horizontal lcolor(black) ||	///
	rspike ul ll pad if var == "FE", horizontal lcolor(black) ||	///
	rspike ul ll pad if var == "HS" , horizontal lcolor(black) ||	///
	scatter junk beta, msymbol(i)	/// make invisible padding
	legend(order(1 "Unweighted" 2 "Random Effects" 3 "Fixed Effects" 4 "Hunter-Schmidt") rows(1) size(small)) xtitle("Effect Size") ytitle("") ///
	yscale(off) xline(0) $texthere  ///
	xlabel(-0.12(.04).16) ysize(4)
	
	*graph export "4categories.pdf", as(pdf) replace
restore



* ---------------------------------------
* for revenue breakout
preserve
keep if inlist(id, 2, 12, 13,14) 
replace id = 4 if id == 2
replace id = 3 if id == 14
replace id = 2 if id == 13
replace id = 1 if id == 12
sort id

gen pad = id+(type/6)
gen junk = pad +.4
global texthere ""
levelsof id, local(levels)
foreach i of local levels	{
	
	if `i' == 1	{
		local tx`i' "Other Revenue (19)"
	}
	if `i' == 2	{
		local tx`i' "Tax Revenue (160)"
	}
	if `i' == 3	{
		local tx`i' "Total Revenue (64)"
	}
	if `i' == 4	{
		local tx`i' "Overall (243)"
	}
	*su beta if id == `i' & var == "FE", meanonly
	*local xpos`i' = r(mean)
	local xpos`i' = .08
	su pad if id == `i' & var == "FE", meanonly
	local ypos`i' = r(mean) 
	global texthere "$texthere text(`ypos`i'' `xpos`i'' "`tx`i''", size(medsmall) )"
}

twoway scatter pad beta if var == "UW", msymbol(T) mcolor(black) || ///
	scatter pad beta if var == "RE", msymbol(O) mcolor(black)	||	///
	scatter pad beta if var == "FE", msymbol(X) mcolor(black) || 	///
	scatter pad beta if var == "HS", msymbol(S) mcolor(black)	||	///
	rspike ul ll pad if var == "UW", horizontal lcolor(black) ||	///
	rspike ul ll pad if var == "RE", horizontal lcolor(black) ||	///
	rspike ul ll pad if var == "FE", horizontal lcolor(black) ||	///
	rspike ul ll pad if var == "HS" , horizontal lcolor(black) ||	///
	scatter junk beta, msymbol(i)	/// make invisible padding
	legend(order(1 "Unweighted" 2 "Random Effects" 3 "Fixed Effects" 4 "Hunter-Schmidt") rows(1) size(small)) xtitle("Effect Size") ytitle("") ///
	yscale(off) xline(0) $texthere  ///
	xlabel(-0.12(.04).12) ysize(4)
	
	*graph export "revenuebreakout.pdf", as(pdf) replace
restore



* ---------------------------------------
* for expenditure breakout
preserve
keep if inlist(id, 1, 5, 6, 7,8,9, 10, 11) 
replace id = 20 if id == 1
replace id = 19 if id == 8
replace id = 18 if id == 11
replace id = 17 if id == 5
replace id = 16 if id == 9
replace id = 15 if id == 6
replace id = 14 if id == 10
replace id = 13 if id == 7
replace id = id - 12 
sort id

gen pad = id+(type/6)
gen junk = pad +.4
global texthere ""
levelsof id, local(levels)
foreach i of local levels	{
	
	if `i' == 1	{
		local tx`i' "Current Expenditure (130)"
	}
	if `i' == 2	{
		local tx`i' "Other (96)"
	}
	if `i' == 3	{
		local tx`i' "Capital Expenditure (171)"
	}
	if `i' == 4	{
		local tx`i' "Health/Education (52)"
	}
	if `i' == 5	{
		local tx`i' "Administrative (55)"
	}
	if `i' == 6	{
		local tx`i' "Total Expenditures (123)"
	}
	if `i' == 7	{
		local tx`i' "Inter-Govt. Grants (72)"
	}
	if `i' == 8	{
		local tx`i' "Overall (699)"
	}
	*su beta if id == `i' & var == "FE", meanonly
	*local xpos`i' = r(mean)
	local xpos`i' = .20
	su pad if id == `i' & var == "FE", meanonly
	local ypos`i' = r(mean) 
	global texthere "$texthere text(`ypos`i'' `xpos`i'' "`tx`i''", size(medsmall) )"
}



twoway scatter pad beta if var == "UW", msymbol(T) mcolor(black) || ///
	scatter pad beta if var == "RE", msymbol(O) mcolor(black)	||	///
	scatter pad beta if var == "FE", msymbol(X) mcolor(black) || 	///
	scatter pad beta if var == "HS", msymbol(S) mcolor(black)	||	///
	rspike ul ll pad if var == "UW", horizontal lcolor(black) ||	///
	rspike ul ll pad if var == "RE", horizontal lcolor(black) ||	///
	rspike ul ll pad if var == "FE", horizontal lcolor(black) ||	///
	rspike ul ll pad if var == "HS" , horizontal lcolor(black) ||	///
	scatter junk beta, msymbol(i)	/// make invisible padding
	legend(order(1 "Unweighted" 2 "Random Effects" 3 "Fixed Effects" 4 "Hunter-Schmidt") rows(1) size(small)) xtitle("Effect Size") ytitle("") ///
	yscale(off ) xline(0) $texthere  ///
	xlabel(-0.04(.04).24) ysize(6)
	
	*graph export "expenditurebreakout.pdf", as(pdf) replace
restore



