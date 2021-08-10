# Analysis in R:
# Andrew Q. Philips
# 8/30/16

# uses the Stata dataset and calculates the effect sizes, and saves them for use in Stata. Also runs the BMA.
library(foreign)
library(metafor)
library(plyr)
library(texreg)
set.seed(2342340)
dat <- read.dta("meta_analysis_data.dta")
attach(dat)
head(dat)
#-------------------------------------------------------------#

#-------------------------------------------------------------#
# Estimating unweighted and weighted averages. Note that weights (excepting unweighted) are based on # observations.  These are to be included in the main paper:
names <- c("dv_expenditures","dv_revenues","dv_debt","dv_fiscalbalance","dv_exp_administrative","dv_exp_capexpenditures","dv_exp_curexpenditures","dv_exp_grantstransfers","dv_exp_healtheducation","dv_exp_other","dv_exp_totexpenditures","dv_rev_otherrevenues","dv_rev_taxrevenue","dv_rev_totrevenue")

output <- cbind(names, NA, NA, NA, NA, NA, NA, NA, NA, NA)
output # container to hold data
specify_decimal <- function(x, k) format(round(x, k), nsmall = k) # program decimals the right way
list <- c("REML","FE","HS") # list to quickly loop through the weighted methods


# For expenditures:
output[1,2] <- rma(yi = parcorr, sei=parcorr_se, method="FE",weighted=FALSE, data=dat, subset= (dv_marker == "dv_expenditures"))$k # get obs
output[1,3] <- specify_decimal(rma(yi = parcorr, sei=parcorr_se, method="FE", weighted = FALSE, data = dat, subset= (dv_marker == "dv_expenditures"))$b, 4) # get b
output[1,4] <- specify_decimal(rma(yi = parcorr, sei=parcorr_se, method="FE", weighted = FALSE, data = dat, subset= (dv_marker == "dv_expenditures"))$se, 4) # get se
i <- 5
for (mthod in list) {
	output[1,i] <- specify_decimal(rma(yi = parcorr, sei = parcorr_se, method= mthod, weights = obs, data = dat, subset = (dv_marker == "dv_expenditures"))$b, 4) # b
	i <- i + 1
	output[1,i] <- specify_decimal(rma(yi = parcorr, sei = parcorr_se, method= mthod, weights = obs, data = dat, subset = (dv_marker == "dv_expenditures"))$se, 4) # se
	i <- i + 1
}

# For revenues:
output[2,2] <- rma(yi = parcorr, sei=parcorr_se, method="FE",weighted=FALSE, data=dat, subset= (dv_marker == "dv_revenues"))$k # get obs
output[2,3] <- specify_decimal(rma(yi = parcorr, sei=parcorr_se, method="FE", weighted = FALSE, data = dat, subset= (dv_marker == "dv_revenues"))$b, 4) # get b
output[2,4] <- specify_decimal(rma(yi = parcorr, sei=parcorr_se, method="FE", weighted = FALSE, data = dat, subset= (dv_marker == "dv_revenues"))$se, 4) # get se
i <- 5
for (mthod in list) {
	output[2,i] <- specify_decimal(rma(yi = parcorr, sei = parcorr_se, method= mthod, weights = obs, data = dat, subset = (dv_marker == "dv_revenues"))$b, 4) # b
	i <- i + 1
	output[2,i] <- specify_decimal(rma(yi = parcorr, sei = parcorr_se, method= mthod, weights = obs, data = dat, subset = (dv_marker == "dv_revenues"))$se, 4) # se
	i <- i + 1
}

# For debt:
output[3,2] <- rma(yi = parcorr, sei=parcorr_se, method="FE",weighted=FALSE, data=dat, subset= (dv_marker == "dv_debt"))$k # get obs
output[3,3] <- specify_decimal(rma(yi = parcorr, sei=parcorr_se, method="FE", weighted = FALSE, data = dat, subset= (dv_marker == "dv_debt"))$b, 4) # get b
output[3,4] <- specify_decimal(rma(yi = parcorr, sei=parcorr_se, method="FE", weighted = FALSE, data = dat, subset= (dv_marker == "dv_debt"))$se, 4) # get se
i <- 5
for (mthod in list) {
	output[3,i] <- specify_decimal(rma(yi = parcorr, sei = parcorr_se, method= mthod, weights = obs, data = dat, subset = (dv_marker == "dv_debt"))$b, 4) # b
	i <- i + 1
	output[3,i] <- specify_decimal(rma(yi = parcorr, sei = parcorr_se, method= mthod, weights = obs, data = dat, subset = (dv_marker == "dv_debt"))$se, 4) # se
	i <- i + 1
}

# For fiscal balance:
output[4,2] <- rma(yi = parcorr, sei=parcorr_se, method="FE",weighted=FALSE, data=dat, subset= (dv_marker == "dv_fiscalbalance"))$k # get obs
output[4,3] <- specify_decimal(rma(yi = parcorr, sei=parcorr_se, method="FE", weighted = FALSE, data = dat, subset= (dv_marker == "dv_fiscalbalance"))$b, 4) # get b
output[4,4] <- specify_decimal(rma(yi = parcorr, sei=parcorr_se, method="FE", weighted = FALSE, data = dat, subset= (dv_marker == "dv_fiscalbalance"))$se, 4) # get se
i <- 5
for (mthod in list) {
	output[4,i] <- specify_decimal(rma(yi = parcorr, sei = parcorr_se, method= mthod, weights = obs, data = dat, subset = (dv_marker == "dv_fiscalbalance"))$b, 4) # b
	i <- i + 1
	output[4,i] <- specify_decimal(rma(yi = parcorr, sei = parcorr_se, method= mthod, weights = obs, data = dat, subset = (dv_marker == "dv_fiscalbalance"))$se, 4) # se
	i <- i + 1
}

# now loop through the list of sub-categories for expenditures and revenues:
list.name <-c( "dv_exp_administrative","dv_exp_capexpenditures","dv_exp_curexpenditures","dv_exp_grantstransfers","dv_exp_healtheducation","dv_exp_other","dv_exp_totexpenditures","dv_rev_otherrevenues","dv_rev_taxrevenue","dv_rev_totrevenue")
p <- 5 # start w/ row 5 in output
for (lname in list.name) {
	output[p,2] <- rma(yi = parcorr, sei=parcorr_se, method="FE",weighted=FALSE, data=dat, subset= (dv_subcat_marker == lname))$k # get obs
	output[p,3] <- specify_decimal(rma(yi = parcorr, sei=parcorr_se, method="FE", weighted = FALSE, data = dat, subset= (dv_subcat_marker == lname))$b, 4) # get b
	output[p,4] <- specify_decimal(rma(yi = parcorr, sei=parcorr_se, method="FE", weighted = FALSE, data = dat, subset= (dv_subcat_marker == lname))$se, 4) # get se
	# now loop through the weighted methods:
	i <- 5
	for (mthod in list) {
		output[p,i] <- specify_decimal(rma(yi = parcorr, sei = parcorr_se, method= mthod, weights = obs, data = dat, subset = (dv_subcat_marker == lname))$b, 4) # b
		i <- i + 1
		output[p,i] <- specify_decimal(rma(yi = parcorr, sei = parcorr_se, method= mthod, weights = obs, data = dat, subset = (dv_subcat_marker == lname))$se, 4) # se
		i <- i + 1
	}
	p <- p + 1
}

colnames(output) <- c("type", "obs", "b unw", "se unw", "b RE", "se RE", "b FE", "se FE", "b HS", "se HS") # add in column names
output # everything looks good
write.foreign(output, datafile="output.csv", codefile="trash_this_file.do", package="Stata") # write output for stata





#	----------------------------------------------------------------#
#----------------------- PART II: MRA     -------------------------
#	----------------------------------------------------------------#
dat <- read.dta("analysis_exp.dta")
attach(dat)
head(dat)
mr.dat <- data.frame(parcorr_se , region_oecd , region_latam , region_asia , region_subsaharan , region_soviet , avg_yr_in_sample , temp_agg_quarterly , temp_agg_monthly , singlecountry , muni_elec , state_elec , control_democracy , control_coalition , control_debt , control_deficit , control_GDP_any , control_GDPgrowth , control_govtexpenditure , control_ideology , control_inflation , control_presidential , control_proportional , control_revenues , control_transfers , control_unemployment , control_winmargin , method_feunit , method_dynamics , method_olspcsegls , elec_dummy , elec_Franzese , elec_half , elec_exogenous , elec_endogenous , control_postelec , control_prelec  , totmodels , cites_peryr , JournalImpactFactor, dv_exp_administrative, dv_exp_capexpenditures, dv_exp_curexpenditures, dv_exp_grantstransfers, dv_exp_healtheducation, dv_exp_totexpenditures)


# Model 1: RE full
rma.re <- rma(yi = parcorr, sei=parcorr_se, method="REML", weighted=TRUE, intercept=TRUE, weights=obs, data = dat,mods = mr.dat)
summary(rma.re)

output.mra <- data.frame(matrix(ncol=8, nrow=47))
output.mra # container to hold data
specify_decimal <- function(x, k) format(round(x, k), nsmall = k) # program decimals the right way
output.mra[,1] <- specify_decimal(rma.re$b, 3) # get b
output.mra[,2] <- specify_decimal(rma.re$se, 3) # get se
output.mra[,3] <- specify_decimal(rma.re$pval, 3) # get pval
output.mra[,4] <- "&"

# Model 2: FE full
rma.fe <- rma(yi = parcorr, sei=parcorr_se, method="FE", weighted=TRUE, intercept=TRUE, weights=obs, data = dat, mods = mr.dat)
summary(rma.fe)
output.mra[,5] <- specify_decimal(rma.fe$b,3) # get b
output.mra[,6] <- specify_decimal(rma.fe$se, 3) # get se
output.mra[,7] <- specify_decimal(rma.fe$pval, 3) # get pval
output.mra[,8] <- "\\"
mr.dat2 <- cbind(mr.dat, "Constant")
output.mra <- cbind(names(mr.dat2), output.mra)
output.mra

# Model 3: BMA full
library(BMS)  # the package for running the
library(ggplot2)
require(graphics)
res <- bms(cbind(parcorr,mr.dat), burn= 500000, iter=2000000, nmodel=500, mprior= "random", g = "BRIC", mcmc="rev.jump" ) # BMA with 500,000 burnins, 2 million iterations, priors that are random. and sampler that jumps in addition to birth/death.
summary(res)
plotConv(res)
plotModelsize(res)
png(filename="pip_plot_revjump.png", width = 1000, height = 800)
image(res) # exported as pip_plot_revjump
dev.off()

# Model 4: Slim RE
mr.dat.slim <- data.frame(method_dynamics, parcorr_se, dv_exp_grantstransfers, control_revenues, control_unemployment, cites_peryr, control_govtexpenditure, state_elec, avg_yr_in_sample, region_subsaharan, dv_exp_totexpenditures,muni_elec,region_asia, method_olspcsegls,elec_dummy,JournalImpactFactor, elec_exogenous, control_postelec,control_transfers,control_democracy,elec_half)
rma.re.slim <- rma(yi = parcorr, sei=parcorr_se, method="REML", weighted=TRUE, intercept=TRUE, weights=obs, data = dat,mods = mr.dat.slim)
summary(rma.re.slim)


# ---------- MRA/BMA for Revenues ------------#
dat <- read.dta("analysis_rev.dta")
attach(dat)
head(dat)
mr.dat <- data.frame(parcorr_se , region_oecd , region_latam , region_asia , region_subsaharan , region_soviet , avg_yr_in_sample , temp_agg_quarterly , temp_agg_monthly , singlecountry , muni_elec , state_elec  , control_coalition , control_debt  , control_GDP_any , control_GDPgrowth , control_govtexpenditure , control_ideology    , control_revenues , control_transfers , control_unemployment  , method_feunit , method_dynamics , method_olspcsegls , elec_dummy , elec_Franzese  , elec_exogenous , elec_endogenous , control_postelec , control_prelec  , totmodels , cites_peryr , JournalImpactFactor, dv_rev_taxrevenue, dv_rev_totrevenue)


# Model 1: RE full
rma.re <- rma(yi = parcorr, sei=parcorr_se, method="REML", weighted=TRUE, intercept=TRUE, weights=obs, data = dat,mods = mr.dat)
summary(rma.re)

output.mra <- data.frame(matrix(ncol=8, nrow=36))
output.mra # container to hold data
specify_decimal <- function(x, k) format(round(x, k), nsmall = k) # program decimals the right way
output.mra[,1] <- specify_decimal(rma.re$b, 3) # get b
output.mra[,2] <- specify_decimal(rma.re$se, 3) # get se
output.mra[,3] <- specify_decimal(rma.re$pval, 3) # get pval
output.mra[,4] <- "&"

# Model 2: FE full
rma.fe <- rma(yi = parcorr, sei=parcorr_se, method="FE", weighted=TRUE, intercept=TRUE, weights=obs, data = dat, mods = mr.dat)
summary(rma.fe)
output.mra[,5] <- specify_decimal(rma.fe$b,3) # get b
output.mra[,6] <- specify_decimal(rma.fe$se, 3) # get se
output.mra[,7] <- specify_decimal(rma.fe$pval, 3) # get pval
output.mra[,8] <- "\\"
mr.dat2 <- cbind(mr.dat, "Constant")
output.mra

# Model 3: BMA full
res <- bms(cbind(parcorr,mr.dat), burn= 500000, iter=2000000, nmodel=500, mprior= "random", g = "BRIC", mcmc="rev.jump" ) # BMA with 500,000 burnins, 2 million iterations, priors that are random. and sampler that jumps in addition to birth/death.
summary(res)
plotConv(res)
plotModelsize(res)
png(filename="pip_plot_revjump_rev.png", width = 1000, height = 800)
image(res) # exported as pip_plot_revjump
dev.off()

# Model 4: Slim RE
mr.dat.slim <- data.frame(method_dynamics, elec_endogenous, elec_dummy, temp_agg_quarterly, totmodels, region_soviet, cites_peryr, control_GDPgrowth)
rma.re.slim <- rma(yi = parcorr, sei=parcorr_se, method="REML", weighted=TRUE, intercept=TRUE, weights=obs, data = dat,mods = mr.dat.slim)
summary(rma.re.slim)



#-------------------Fiscal balance --------------#
dat <- read.dta("analysis_fb.dta")
attach(dat)
head(dat)
mr.dat <- data.frame(parcorr_se , region_oecd , region_latam , region_asia , region_subsaharan , region_soviet , avg_yr_in_sample , temp_agg_quarterly , temp_agg_monthly , singlecountry , muni_elec , state_elec , control_democracy , control_coalition , control_debt  , control_GDP_any , control_GDPgrowth  , control_ideology , control_inflation , control_presidential  , control_revenues , control_transfers , control_unemployment , control_winmargin , method_feunit , method_dynamics , method_olspcsegls , elec_dummy , elec_Franzese , elec_half , elec_exogenous , elec_endogenous , control_postelec , control_prelec  , totmodels , cites_peryr , JournalImpactFactor)


# Model 1: RE full
rma.re <- rma(yi = parcorr, sei=parcorr_se, method="REML", weighted=TRUE, intercept=TRUE, weights=obs, data = dat,mods = mr.dat)
summary(rma.re)

output.mra <- data.frame(matrix(ncol=8, nrow=38))
output.mra # container to hold data
specify_decimal <- function(x, k) format(round(x, k), nsmall = k) # program decimals the right way
output.mra[,1] <- specify_decimal(rma.re$b, 3) # get b
output.mra[,2] <- specify_decimal(rma.re$se, 3) # get se
output.mra[,3] <- specify_decimal(rma.re$pval, 3) # get pval
output.mra[,4] <- "&"

# Model 2: FE full
rma.fe <- rma(yi = parcorr, sei=parcorr_se, method="FE", weighted=TRUE, intercept=TRUE, weights=obs, data = dat, mods = mr.dat)
summary(rma.fe)
output.mra[,5] <- specify_decimal(rma.fe$b,3) # get b
output.mra[,6] <- specify_decimal(rma.fe$se, 3) # get se
output.mra[,7] <- specify_decimal(rma.fe$pval, 3) # get pval
output.mra[,8] <- "\\"
output.mra

# Model 3: BMA full
res <- bms(cbind(parcorr,mr.dat), burn= 500000, iter=2000000, nmodel=500, mprior= "random", g = "BRIC", mcmc="rev.jump" ) # BMA with 500,000 burnins, 2 million iterations, priors that are random. and sampler that jumps in addition to birth/death.
summary(res)
plotConv(res)
plotModelsize(res)
png(filename="pip_plot_revjump_fb.png", width = 1000, height = 800)
image(res) # exported as pip_plot_revjump
dev.off()

# Model 4: Slim RE
mr.dat.slim <-  data.frame(method_dynamics,state_elec,muni_elec,singlecountry)
rma.re.slim <- rma(yi = parcorr, sei=parcorr_se, method="REML", weighted=TRUE, intercept=TRUE, weights=obs, data = dat,mods = mr.dat.slim)
summary(rma.re.slim)
