README

Andrew Q. Philips
8/30/16

—This document shows the order in which you run the processing files for “Seeing the forest through the trees: A meta-analysis of political budget cycles,” forthcoming at Public Choice.

1.—analysis 1.do
	—input: uses “meta_analysis_data.dta” to create graphs and label things from “dataset.xls”
	—creates funnel plots
	—obtains summary statistics

2.—analysis 2.R
	—input: uses “analysis v2-1R.dta” and the _exp, _fb, and _rev datasets
	—calculates effect sizes across the various categories
	—Part II: runs bayesian model averaging for meta-regression section
	—output: saves “output.csv” which is the calculated effect size weighted by the # of observations. For use in the main paper. 

3.-analysis 3.do
	-uses output.csv file to create plots for main paper and for the SI.
