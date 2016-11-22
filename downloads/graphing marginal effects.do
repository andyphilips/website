*	---------------------
*   Andy Philips
*   Texas A&M Univ
*	aphilips@pols.tamu.edu
*   2/13/2015

/*	---------------------
   I find default Stata graphs rather ugly. But it does not need to be that way!
	like R, Stata has a lot of options to alter the presentation of graphs. This
	file shows how to alter marginal effects plots to something relatively ready 
	for publication.
*/

*	--------------------------------------------------------------
*	--------------------------------------------------------------


/* note that these graphs are made using the the default s2color. I prefer
	using the burd scheme which is located at
	http://fmwww.bc.edu/RePEc/bocode/s and can be downloaded by typing: findit
	burd
*/
set scheme s2color	// the default stata theme
graph set window fontface "GillSans"	// turns default graph font into GillSans


* create some data that have an interactive relationship:
clear
set seed 19203
set obs 40
gen x1 = rnormal()
gen x2 = rnormal(,10)
kdensity x1
kdensity x2
su x*

gen y = 100 + 6*x1 - 10*x2 - .5*x1*x2 + rnormal()
reg y c.x1##c.x2

* run margins before marginsplot; we want to see marginal effects of X1 -> Y
margins, dydx(x1) at(x2 = (8.4(.4)11.8))

marginsplot	// the basic plot


/* let's clean this up. First, let's get rid of most of the gray in the graph,
	adding a y-line at 0 to examine statistical significance. Second, we can
	take away the distracting big dots, along with the horizontal significance
	bars. Third, we rotate the y-labels so they're not sideways. Finally, we 
	rename the axes and the title. */


marginsplot,	///
	 graphregion(color(white))	/// get rid of the gray outerbox
	 ci1opts(recast(rspike))	/// horizontal ci bars distract
	 recast(line)	/// get rid of the big dots
	 yline(0, lpattern(dash)	/// add dash horizontal line at 0
	 lwidth(vvthin) lcolor(black))	/// make line thin, black
	 ylabel( , angle(0) nogrid)	/// horiz. y-axis labels & no grid
		/// alternatively could specify VERY light lines w/ glcolor(gs15)
	 xtitle("X2")	/// x-axis title
	 ytitle("Marginal Effect of X1")	/// y-axis title
	 title("Marginal Effect of X1 on Y")	/// main title
	 note("95% confidence intervals")	// small note of confidence int.
	 
	 
/*	If we wanted, we could get rid of the confidence interval spike lines and
	replace them with a smooth area:
*/
marginsplot,	///
	 graphregion(color(white))	/// get rid of the gray outerbox
	 ci1opts(recast(rarea) fintensity(15))	/// horizontal ci bars distract
	 recast(line)	/// get rid of the big dots
	 yline(0, lpattern(dash)	/// add dash horizontal line at 0
	 lwidth(vvthin) lcolor(black))	/// make line thin, black
	 ylabel( , angle(0) nogrid)	/// horiz. y-axis labels & no grid
		/// alternatively could specify VERY light lines w/ glcolor(gs15)
	 xtitle("X2")	/// x-axis title
	 ytitle("Marginal Effect of X1")	/// y-axis title
	 title("Marginal Effect of X1 on Y")	/// main title
	 note("95% confidence intervals")	// small note of confidence int.
	 
	 
/* or we could keep just the confidence intervals: */
marginsplot,	///
	 graphregion(color(white))	/// get rid of the gray outerbox
	 ci1opts(recast(rline) fintensity(15) lpattern(dash_3dot)) /// add dash
	 recast(line)	/// get rid of the big dots
	 yline(0, lpattern(dash)	/// add dash horizontal line at 0
	 lwidth(vvthin) lcolor(black))	/// make line thin, black
	 ylabel( , angle(0) nogrid)	/// horiz. y-axis labels & no grid
		/// alternatively could specify VERY light lines w/ glcolor(gs15)
	 xtitle("X2")	/// x-axis title
	 ytitle("Marginal Effect of X1")	/// y-axis title
	 title("Marginal Effect of X1 on Y")	/// main title
	 note("95% confidence intervals")	// small note of confidence int.
	 
	 
	 
/* Overall, the rarea graph uses up a lot of color and is as substantively 
	meaningful as having the simpler vertical lines, or the dashes. In addition,
	 this plot doesn't tell anything about the underlying distribution of X2. One 	way is to add a kernel density behind the plot: */
	
marginsplot,	///
	 graphregion(color(white))	/// get rid of the gray outerbox
	 ci1opts(recast(rline) fintensity(15) lpattern(dash_3dot)) /// add dash
	 recast(line)	/// get rid of the big dots
	 yline(0, lpattern(dash)	/// add dash horizontal line at 0
	 lwidth(vvthin) lcolor(black))	/// make line thin, black
	 ylabel( , angle(0) nogrid)	/// horiz. y-axis labels & no grid
		/// alternatively could specify VERY light lines w/ glcolor(gs15)
	 xtitle("X2")	/// x-axis title
	 ytitle("Marginal Effect of X1")	/// y-axis title
	 title("Marginal Effect of X1 on Y")	/// main title
	 note("95% confidence intervals")	/// small note of confidence int.
	 addplot(kdensity x2, color(gs14)	/// add the density
	 yaxis(2) yscale(off axis(2)))	/// give it an invisible axis
	 xlabel(8(1)12)	/// re-label the x-axis
	 legend(off)	//  keep it from showing on legend


/* if we wanted to show the distribution of X2 but not on the plot area, we 
	could create a rug plot by the following: */
gen pipe = "|"	
gen below = -1	// want this to be just below the lowest CI bar

marginsplot,	///
	 graphregion(color(white))	/// get rid of the gray outerbox
	 ci1opts(recast(rline) fintensity(15) lpattern(dash_3dot)) /// add dash
	 recast(line)	/// get rid of the big dots
	 yline(0, lpattern(dash)	/// add dash horizontal line at 0
	 lwidth(vvthin) lcolor(black))	/// make line thin, black
	 ylabel( , angle(0) nogrid)	/// horiz. y-axis labels & no grid
	 /// alternatively could specify VERY light lines w/ glcolor(gs15)
	 xtitle("X2")	/// x-axis title
	 ytitle("Marginal Effect of X1")	/// y-axis title
	 title("Marginal Effect of X1 on Y")	/// main title
	 note("95% confidence intervals")	/// small note of confidence int.
	 addplot(scatter below x2,	/// add the rug plot
	 msymbol(none) mlabel(pipe))	/// add the label to rug plot
	 xlabel(8(1)12)	/// re-label the x-axis
	 legend(off)	//  keep it from showing on legend


/* We could add both the density plot and the rug plot, although the plot
	starts to get crowded: */
	
	
marginsplot,	///
	 graphregion(color(white))	/// get rid of the gray outerbox
	 ci1opts(recast(rline) fintensity(15) lpattern(dash_3dot)) /// add dash
	 recast(line)	/// get rid of the big dots
	 yline(0, lpattern(dash)	/// add dash horizontal line at 0
	 lwidth(vvthin) lcolor(black))	/// make line thin, black
	 ylabel( , angle(0) nogrid)	/// horiz. y-axis labels & no grid
		/// alternatively could specify VERY light lines w/ glcolor(gs15)
	 xtitle("X2")	/// x-axis title
	 ytitle("Marginal Effect of X1")	/// y-axis title
	 title("Marginal Effect of X1 on Y")	/// main title
	 note("95% confidence intervals")	/// small note of confidence int.
	 addplot(scatter below x2,	/// add the rug plot
	 msymbol(none) mlabel(pipe) ||	/// add label to rug plot
	 kdensity x2, color(gs14)	/// add the density
	 yaxis(2) yscale(off axis(2)))	///
	 xlabel(8(1)12)	/// re-label the x-axis
	 legend(off)	//  keep it from showing on legend


/* when working with marginal effects, remember the symmetry of the interaction.
	We can show both the marginal effect of X1 on Y (given X2) and the marginal
	effect of X2 on Y (given X1) by creating two separate graphs and combining 
	them. First the marginal effect of X1 on Y
*/

tempname X1onY X2onY

margins, dydx(x1) at(x2 = (8.4(.4)11.8))
marginsplot,	///
	 graphregion(color(white))	/// get rid of the gray outerbox
	 ci1opts(recast(rline) fintensity(15) lpattern(dash_3dot)) /// add dash
	 recast(line)	/// get rid of the big dots
	 yline(0, lpattern(dash)	/// add dash horizontal line at 0
	 lwidth(vvthin) lcolor(black))	/// make line thin, black
	 ylabel( , angle(0) nogrid)	/// horiz. y-axis labels & no grid
		/// alternatively could specify VERY light lines w/ glcolor(gs15)
	 xtitle("X2")	/// x-axis title
	 ytitle("Marginal Effect of X1")	/// y-axis title
	 title("Marginal Effect of X1 on Y")	/// main title
	 note("95% confidence intervals")	/// small note of confidence int.
	 addplot(scatter below x2,	/// add the rug plot
	 msymbol(none) mlabel(pipe))	/// add the label to rug plot
	 xlabel(8(1)12)	/// re-label the x-axis
	 legend(off)	/// keep it from showing on legend
	 saving(`X1onY', replace)	//  temporarily save
	
	
* now get the marginal effect of X2 on Y:
margins, dydx(x2) at(x1 = (-1.6(.2)1.6))
gen below2 = -11
marginsplot,	///
	 graphregion(color(white))	/// get rid of the gray outerbox
	 ci1opts(recast(rline) fintensity(15) lpattern(dash_3dot)) /// add dash
	 recast(line)	/// get rid of the big dots
	 ///yline(0, lpattern(dash)	/// add dash horizontal line at 0
	 ///lwidth(vvthin) lcolor(black))	/// make line thin, black
	 ylabel( , angle(0) nogrid)	/// horiz. y-axis labels & no grid
	 /// alternatively could specify VERY light lines w/ glcolor(gs15)
	 xtitle("X1")	/// x-axis title
	 ytitle("Marginal Effect of X2")	/// y-axis title
	 title("Marginal Effect of X2 on Y")	/// main title
	 note("95% confidence intervals")	/// small note of confidence int.
	 addplot(scatter below2 x1,	/// add the rug plot
	 msymbol(none) mlabel(pipe))	/// add the label to rug plot
	 xlabel(-2(.25)2)	/// re-label the x-axis
	 legend(off)	/// keep it from showing on legend
	 saving(`X2onY', replace)	//  temporarily save
	
graph combine `X1onY'.gph `X2onY'.gph ,  graphregion( color(white) ) col(1)

* if we wanted to save this graph we could export it as an .eps by un-commenting
* the following line:
* graph export "mymarginaleffects.eps", as(eps) preview(off) replace




/* Finally, I prefer the user-written burd scheme, which makes some of 
	these options the default.
*/
set scheme burd
margins, dydx(x1) at(x2 = (8.4(.4)11.8))
marginsplot,	///
	 recast(line)	/// get rid of the big dots
	 ci1opts(recast(rline) fintensity(15) lpattern(dash_3dot)) /// add dash
	 yline(0, lcolor(black))	///
	 xtitle("X2")	/// x-axis title
	 ytitle("Marginal Effect of X1")	/// y-axis title
	 title("Marginal Effect of X1 on Y")	/// main title
	 note("95% confidence intervals")	/// small note of confidence int.
	 addplot(scatter below x2,	/// add the rug plot
	 msymbol(none) mlabel(pipe))	/// add the label to rug plot
	 xlabel(8(1)12)	/// re-label the x-axis
	 legend(off)	// keep it from showing on legend



/*  Examining non-continuous interactions -----------------------------------
	the graphs above examined the marginal effects of 2 continuous variables.
	if we interact a dummy variable with a continuous variable, we may want
	to instead plot the predicted effect */
	
gen dummy = 0
replace dummy = 1 if x2 > 10.17
gen y2 = 100 + 6*x1 - 10*x2 + 20*dummy - 15*x1*dummy + rnormal()
reg y2 x2 c.x1##i.dummy

* run margins before marginsplot; we want to see the effect of the dummy -> Y 
* over x1:
margins i.dummy, at(x1 = (-1.9(.3)1.9))

marginsplot


* we can also clean this up a bit:
marginsplot,	///
	 graphregion(color(white))	/// get rid of the gray outerbox
	 ci1opts(recast(rline) fintensity(15) lpattern(dash_3dot)) /// add dash
	 recast(line)	/// get rid of the big dots
	 yline(0, lpattern(dash)	/// add dash horizontal line at 0
	 lwidth(vvthin) lcolor(black))	/// make line thin, black
	 ci2opts(recast(rline) fintensity(15) lpattern(dash)) /// dash for line 2
	 ylabel( , angle(0) nogrid)	/// horiz. y-axis labels & no grid
	 xtitle("X1")	/// x-axis title
	 ytitle("Predicted Y")	/// y-axis title
	 title("Predicted Effect of X1 on Y")	/// main title
	 note("95% confidence intervals") /// small note of confidence int.
	 xlabel(-2(1)2)	/// re-label the x-axis
	  legend( order(1 "dummy" 2 "no dummy") ring(0) pos(1))	//  relabel legend

