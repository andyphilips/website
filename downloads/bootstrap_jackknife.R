# Andy Philips
# Texas A&M Univ.
# 2/03/15

# This program examines bootstrapping and jackknifing in R


# Bootstrap -----------------------------------------------------------
#-----------------------------------------------------------------------
# always set a seed
set.seed(23434)

x1 <- rnorm(40,0,1) # generate 40 obs, mean = 0, sd = 1
x2 <- rnorm(40,0,1)
e <- rnorm(40)
y <- 10 + x1*2 -5*x2 + e

reg.1 <- lm(y~x1 + x2) 	# run a simple linear regression
summary(reg.1)

# -----------------------------------------------------------------------
# we can install the "boot" package, which automates the procedure:
# see http://www.statmethods.net/advstats/bootstrapping.html for some helpful refererence on usage.
#install.packages("boot")
library(boot)

my.data <- as.data.frame(cbind(y,x1,x2)) # we will need a dataset.

# As above, we are trying to bootstrap our 95% confidence intervals. the bootstrap program works by first writing out a function to call to the regression results, then running the boot command, then analyzing the results.
bootstrap <- function(formula, data, regressors) {
	dat <- data[regressors,]					# grabs the sample
	reg <- lm(formula, data = dat)		# runs the regression
	return(coef(reg))									# we need these coefficients
}

# now we run 1000 reps on our regression using my.data, and get the bootstrap statistic written above
bs.res <- boot(R=1000, formula = y~x1 + x2 , data = my.data, statistic = bootstrap)
summary(bs.res)  # note everything that is saved
bs.res$t0        # index (see below) calls from here
?boot.ci        # note that there are 5 different types of CI outputs we can use. Below are four of them

#basic bootstrap
bs.basic.x1 <- boot.ci(bs.res, type="basic", index=2) # 95% for variable x1
bs.basic.x1
bs.basic.x2 <- boot.ci(bs.res, type="basic", index=3) # 95% for variable x2
bs.basic.x2
# adjusted bootstrap percentile (BCa)
bs.bca.x1 <- boot.ci(bs.res, type="bca", index=2)
bs.bca.x1
bs.bca.x2 <- boot.ci(bs.res, type="bca", index=3)
bs.bca.x2

# normal
bs.norm.x1 <- boot.ci(bs.res, type="norm", index=2)
bs.norm.x1
bs.norm.x2 <- boot.ci(bs.res, type="norm", index=3)
bs.norm.x2

# percentile interval
bs.perc.x1 <- boot.ci(bs.res, type="perc", index=2)
bs.perc.x1
bs.perc.x2 <- boot.ci(bs.res, type="perc", index=3)
bs.perc.x2


# let's plot these CIs to see how they differ. Note that I'm pulling the upper and lower CIs from the bs.method.x1$method call. The location of the ul and ll DIFFERS across the method!

# For X1:
plot(NULL, type="l", xlim=c(1,3),ylim=c(0,5), ylab=NA, axes=FALSE, xlab=NA)
  lines(c(bs.basic.x1$basic[4], bs.basic.x1$basic[5]), c(1,1))  # add the 95% confidence intervals
	text(2, 1.2, "Basic", xpd = T, cex = .8)     # add the names
	lines(c(bs.norm.x1$norm[2], bs.norm.x1$norm[3]), c(2,2))
	text(2, 2.2, "Normal", xpd = T, cex = .8)
  lines(c(bs.bca.x1$bca[4], bs.bca.x1$bca[5]), c(3,3))
	text(2, 3.2, "BCa", xpd = T, cex = .8)
	lines(c(bs.perc.x1$perc[4], bs.perc.x1$perc[5]), c(4,4))
	text(2, 4.2, "Percentile", xpd = T, cex = .8)
	abline(v = 2.0, lty = 3, col = "black") #  add vertical line at x1
	axis(side = 1)  # add x axis
	mtext(side = 2, "Bootstrap Method", line = -3)    # label side
	mtext(side = 3, "Performance of Various Bootstrapping Methods for X1", line = 1)   # add a title




# For X2:
plot(NULL, type="l", xlim=c(-6,-4),ylim=c(0,5), ylab=NA, axes=FALSE, xlab=NA)
lines(c(bs.basic.x2$basic[4], bs.basic.x2$basic[5]), c(1,1))  # add the 95% confidence intervals
text(-5, 1.2, "Basic", xpd = T, cex = .8)     # add the names
lines(c(bs.norm.x2$norm[2], bs.norm.x2$norm[3]), c(2,2))
text(-5, 2.2, "Normal", xpd = T, cex = .8)
lines(c(bs.bca.x2$bca[4], bs.bca.x2$bca[5]), c(3,3))
text(-5, 3.2, "BCa", xpd = T, cex = .8)
lines(c(bs.perc.x2$perc[4], bs.perc.x2$perc[5]), c(4,4))
text(-5, 4.2, "Percentile", xpd = T, cex = .8)
abline(v = -5.0, lty = 3, col = "black") #  add vertical line at x2
axis(side = 1)  # add x axis
mtext(side = 2, "Bootstrap Method", line = -3)    # label side
mtext(side = 3, "Performance of Various Bootstrapping Methods for X2", line = 1)   # add a title



# ---------------------------------------------------------------------#
# Jackknife procedure -------------------------------------------------

# Jackknifing can be useful for analyzing if influential observations are affecting our estimates. It works by using a leave-one-out iterating process.

library(bootstrap) # load up the bootstrap package
?jackknife # note that we need to have a vector that we select from the data, as well as some "theta" function that we specify that this function calls to.

x1[10] <-10 # replace the 10th x1 observation with 10
my.data <- as.data.frame(cbind(y,x1,x2)) # we will need a dataset.

plot(hist(x1))
reg.jack <- lm(y ~ x1 + x2)
summary(reg.jack) # notice how much our coefficient estimate has changed (was ~2, now ~0.8)


# now we create the theta function. we specify the leave-one-out, data, and coefficient we would like:
theta <- function(x, dat, coefficient){
	coef(lm(reg.jack , data=dat[x,]))[coefficient] }

# now we run the jackknife, using the theta program, to get the x1 coeff.
res <- jackknife(1:length(x1), theta, dat=my.data, coefficient="x1")
summary(res)
res # we get an output of the jackknifed s.e., the bias, as well as values for each individual observation. Note the observation on 10:
plot(hist(res$jack.values))




# Although jackknifing could be considered more intuitive, it is an older easier (on the computing power) method that bootstrapping encompasses. Bootstrapping should probably be used, although the jackknife is good as examining those outliers.

