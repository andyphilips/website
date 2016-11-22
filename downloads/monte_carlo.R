# Andy Philips
# Texas A&M
# 2/3/15
#	Monte Carlo Simulation
# ---------------------------------------------------------------------------------------

# We are going to examine Monte Carlo Simulations using 2 examples.


# MCs simulate finite data from a hypothesized model...usually we want to examine our confidence intervals to verify that they are the `true' intervals

# This first example comes straight from Cameron and Trivedi that I have adapted for R. It is the Central Limit Theorem in action: our sample mean will converge on the population mean as N -> infinity and approximate a normal distribution.
# ---------------------------------------------------------------

set.seed(463459) # always set a seed

x <- runif(30,0,1)# drawing 30 obs from uniform, min=0, max=1
summary(x)
hist(x)
dens <- density(x)
plot(dens, main="")
polygon(dens,col="cornflowerblue", border="darkgray") # makes it pretty
#here are all the colors available in-house in R:
colors()

# so not a perfect uniform distribution...but definitely NOT a normal distribution either. What happens when we repeat sampling x over thousands of simulations?  The mean of a (0,1) uniform is 0.5, the standard deviation will be sqrt(1/12), and the standard deviation of the sample-mean estimator is sqrt((1/12)/30) since we have 30 obs.  */

reps = 10000
n = 30
results = matrix(NA,nrow=reps,ncol=1)	# this is where output is going
i	= 1
for(i in 1:reps) {
		x <-runif(n,0,1)
		results[i,1] <- mean(x)
}

summary(results)	# summarize our output
hist(results[,1])
dens <- density(results[,1])
plot(dens, main="CLT at Work")
polygon(dens,col="wheat", border="wheat4")
# looks much more like a i.i.d density converging on 0.5 (the "true" mean) even though we are drawing uniform distributions.




# -------------------------------------------------------
# The second example concerns Autoregressive processes. What if our DGP is a lagged dependent variable, but we: a.) continue to use OLS b.) think we have autoregressive errors and estimate using GLS c.) specify a LDV model?

set.seed(68396)

obs = 120
phi = 0.6
epsilon <- rnorm(120,)	# a normal error
y <- rnorm(120,)			# our DV, normally distributed
x <- runif(120,)				# our IV, uniformly distributed

# now create the LDV process. phi = 0.6, and x has a true parameter value of 2.  we also include a constant of 1.
for(i in 2:obs)	{
	y[i] <- 1 + 2*x[i] + phi*y[i-1] + epsilon[i]
}
y <- y[c(2:obs)]# we want to drop out that first observation
epsilon <- epsilon[c(2:obs)]
x <- x[c(2:obs)]
plot(y, col="cornflowerblue")
# hard to see what's going on, so here is the time series line plot:
ts.plot(y, main="Autoregressive Series, phi = 0.6", col="cornflowerblue")

# how does an OLS regression of an AR process compare to other models? lets compare the two.  First, a static OLS regression where we do not take into account the AR process
reg <- lm(y ~ x)
summary(reg)
resids <- reg$residuals
require(graphics)
plot.ts(resids, main="Residuals: Naiive OLS")

# let's look at the relationship between y and the resids:
plot(resids, y, col="orange3" ,main="Y vs. Residuals" )
abline(lm(y~resids))

library(lmtest)	# load up the lmtest package
bgtest(reg, order = 1)	# Breusch-Godfrey, Ho = No AR. This can test for higher order AR(p)
dwtest(reg)	# Remember that 2 = no AR

# What makes this better? One approach would be to use a GLS method such as the Prais Winsten transformation to account for serial correlation. This method is now somewhat defunct, and since I can't find a Prais Winsten canned procedure, I took the one from John Fox and modified it a bit:

prais.winsten <- function(mod, ...){
     UseMethod("prais.winsten")
     }

prais.winsten.lm <- function(mod){
     X. <- model.matrix(mod)
     y. <- model.response(model.frame(mod))
     e. <- residuals(mod)
     n. <- length(e.)
     names <- colnames(X.)
     phi. <- sum(e.[1:(n.-1)]*e.[2:n.])/sum(e.^2)
     y. <- c(y.[1] * (1 - phi.^2)^0.5, y.[2:n.] - phi. * y.[1:(n.-1)])
     X. <- rbind(X.[1,] * (1 - phi.^2)^0.5, X.[2:n.,] - phi. * X.[1:(n.-1),])
     mod <- lm(y. ~ X. - 1)
     result <- list()
     result$coefficients <- coef(mod)
     names(result$coefficients) <- names
     summary <- summary(mod, corr = F)
     result$cov <- (summary$sigma^2) * summary$cov.unscaled
     dimnames(result$cov) <- list(names, names)
     result$sigma <- summary$sigma
     result$phi. <- phi.
     result$residuals <- summary$residuals
     result$stderror <- summary.lm(mod)$coefficient[2,2] # I've had to grab these via an un-orthodox way for the Monte Carlo
     result$df <-summary$df[2]
     class(result) <- 'prais.winsten'
     result
     }

reg2 <- prais.winsten.lm(lm(y~x))
summary(reg2)
reg2
resids2 <- reg2$residuals
plot.ts(resids2, main="Residuals: Prais-Winsten GLS")
plot(resids2, y, col="orange3" ,main="Y vs. Residuals" )
abline(lm(y~resids))

# Note that we could always model the process with a lagged DV (as long as we were sure this was an AR process and not a unit root...ie. phi=1. Here is a shift function to make a lagged DV:
shift<-function(x,shift_by){
	stopifnot(is.numeric(shift_by))
	stopifnot(is.numeric(x))
	if (length(shift_by)>1)
		return(sapply(shift_by,shift, x=x))
	out<-NULL
	abs_shift_by=abs(shift_by)
	if (shift_by > 0 )
		out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
	else if (shift_by < 0 )
		out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
	else
		out<-x
		out
}
ly <- shift(y, -1)
 y
ly
reg3 <- lm(y ~ ly + x)
summary(reg3)
resids3 <- reg3$residuals
plot.ts(resids3, main="Residuals: OLS w/ LDV")
plot(resids3, y[-1], col="orange3" ,main="Y vs. Residuals" )
abline(lm(y~resids))

# here are all the residuals
plot(resids, y, col="green", main="Residuals: All")
points(resids2, y, col="orange3")
points(resids3, y[-1], col="blue")


# we can use Monte Carlos to examine the conditions (of phi) under which each may be preferred. How high must the AR phi be before we reject the null of the parameter on X, H_0: b1 = 2 ? In effect, what are the finite sample properties of OLS as phi goes to 1?
#install.packages("stats", dependencies=TRUE)
#install.packages("car", dependencies=TRUE)

library(car)	# load up the car library
library(stats)
reps = 1	  # we will only loop once to double check it works
obs = 30
phi = 0.6
x.true = 2	# the true value of X from the DGP

results = data.frame(matrix(NA, nrow=reps, ncol=12)) # this is where output is going
dim(results)

results


# Here is the Monte Carlo:	-------------------------------
i <- 0
for(i in 1:reps) {						# start the loop
	i <- i+1
	n <- i
	epsilon <- rnorm(obs,)	# a normal error
	y <- rnorm(obs,)			# our DV
	x <- runif(obs,-3,3)		# uniform x between -3 and 3
# now create the AR process. phi = phi, and x has a true parameter value of 2.
	for(m in 2:obs)	{
		y[m] <- 0 + x.true*x[m] + phi*y[m-1] + epsilon[m]
	}

	ly<-shift(y, -1)
	# OLS MODEL
	ols <- lm(y ~ x)
	b.ols <- summary.lm(ols)$coefficient[2,1]	# get the coefficient of x
	results[i,1] <- b.ols								# put it in our matrix
	se.ols <- summary.lm(ols)$coefficient[2,2]	# get the s.e. of x
	results[i,2] <- se.ols
	df.ols <- summary.lm(ols)$df[2]
	test.ols<- (b.ols-x.true)/se.ols
	pv.ols <- 2*pt(abs(test.ols), df.ols)
	results[i,3] <- pv.ols
	rmse.ols <- sqrt(mean((ols$residuals)^2))# get rmse
	results[i,4] <- rmse.ols

	# PRAIS MODEL
	prais <- prais.winsten.lm(lm(y~x))
	b.prais <- prais$coefficient[2]
	results[i,5] <- b.prais
	se.prais <- prais$stderror
	results[i,6] <- se.prais
	df.prais <- prais$df
	test.prais <- (b.prais-x.true)/se.prais
	pv.prais  <- 2*pt(abs(test.prais), df.prais)
	results[i,7] <- pv.prais
	rmse.prais <- sqrt(mean((prais$residuals)^2))# get rmse
	results[i,8] <- rmse.prais

	# OLS LDV MODEL
	ldv <- lm(y ~  x +  ly)
	b.ldv <- summary.lm(ldv)$coefficient[2,1]
	results[i,9] <- b.ldv
	se.ldv <- summary.lm(ldv)$coefficient[2,2]	# get the s.e. of x
	results[i,10] <- se.ldv
	df.ldv <- summary.lm(ldv)$df[2]
	test.ldv <- (b.ldv-x.true)/se.ldv
	pv.ldv <- 2*pt(abs(test.ldv), df.ldv)
	results[i,11] <- pv.ldv
	rmse.ldv <- sqrt(mean((ldv$residuals)^2))# get rmse
	results[i,12] <- rmse.ldv
}
names(results) <- c("b.ols", "se.ols","pv.ols", "rmse.ols", "b.prais", "se.prais", "pv.prais",  "rmse.prais", "b.ldv", "se.ldv", "pv.ldv",  "rmse.ldv")
results

# that worked. Now make a program to run this thing for any number of obs and reps:


# program my.ldv:
my.ldv <- function(reps, obs, phi, x.true)	{
	results = data.frame(matrix(NA, nrow=reps, ncol=12)) # reset our data-frame
	i <- 0
	for(i in 1:reps) {
		i <- i+1
	n <- i
	epsilon <- rnorm(obs,)	# a normal error
	y <- rnorm(obs,)			# our DV
	x <- runif(obs,-3,3)		# uniform x between -3 and 3
# now create the AR process. phi = phi, and x has a true parameter value of 2.
	for(m in 2:obs)	{
		y[m] <- 0 + x.true*x[m] + phi*y[m-1] + epsilon[m]
	}
	ly<-shift(y, -1)
	# OLS MODEL
	ols <- lm(y ~ x)
	b.ols <- summary.lm(ols)$coefficient[2,1]	# get the coefficient of x
	results[i,1] <- b.ols								# put it in our matrix
	se.ols <- summary.lm(ols)$coefficient[2,2]	# get the s.e. of x
	results[i,2] <- se.ols
	df.ols <- summary.lm(ols)$df[2]
	test.ols<- (b.ols-x.true)/se.ols
	#pv.ols <- 2*pt(abs(test.ols), df.ols)
	results[i,3] <- test.ols
	rmse.ols <- sqrt(mean((ols$residuals)^2))# get rmse
	results[i,4] <- rmse.ols

	# PRAIS MODEL
	prais <- prais.winsten.lm(lm(y~x))
	b.prais <- prais$coefficient[2]
	results[i,5] <- b.prais
	se.prais <- prais$stderror
	results[i,6] <- se.prais
	df.prais <- prais$df
	test.prais <- (b.prais-x.true)/se.prais
	#pv.prais  <- 2*pt(abs(test.prais), df.prais)
	results[i,7] <- test.prais
	rmse.prais <- sqrt(mean((prais$residuals)^2))# get rmse
	results[i,8] <- rmse.prais

	# OLS LDV MODEL
	ldv <- lm(y ~  x +  ly)
	b.ldv <- summary.lm(ldv)$coefficient[2,1]
	results[i,9] <- b.ldv
	se.ldv <- summary.lm(ldv)$coefficient[2,2]	# get the s.e. of x
	results[i,10] <- se.ldv
	df.ldv <- summary.lm(ldv)$df[2]
	test.ldv <- (b.ldv-x.true)/se.ldv
	#pv.ldv <- 2*pt(abs(test.ldv), df.ldv)
	results[i,11] <- test.ldv
	rmse.ldv <- sqrt(mean((ldv$residuals)^2))# get rmse
	results[i,12] <- rmse.ldv
	}
	names(results) <- c("b.ols", "se.ols","test.ols", "rmse.ols", "b.prais", "se.prais", "test.prais",  "rmse.prais", "b.ldv", "se.ldv", "test.ldv",  "rmse.ldv")
	results <- results[-1,]# cut that first missing obs
	 return(results)
}


system.time(monte.carlo <- my.ldv(reps=1000,obs=120, phi=0.90, x.true=2))
# the system.time() is not needed but is nice to see how long it takes to run. 1st column is CPU time, last is total time.
head(monte.carlo) # these are our results
summary(monte.carlo)



library(ggplot2) # this package makes the best graphs in R:

# create id names (excuse my poor R skills!)
names <- rep("OLS", length=3000)
names <- replace(names, 1001:2000 , "Prais" )
names <- replace(names, 2001:3000, "LDV")
names
# create a long dataset of our beta values
data <- data.frame(Method <- as.factor(names) , Beta <- c(monte.carlo$b.ols, monte.carlo$b.prais, monte.carlo$b.ldv))

# Density plot of the Betas
plot1 <- ggplot(data, aes(x=Beta, colour=Method) ) + geom_density() + geom_vline(aes(xintercept = 2), color="red", linetype="dashed", size=1)
plot1

# Same but Filled:
plot2 <- ggplot(data, aes(x=Beta, fill=Method) ) + geom_density(alpha=.5) + geom_vline(aes(xintercept = 2), color="red", linetype="dashed", size=1)
plot2
plot2 +  theme_bw() # remove the gray ugliness


# this one is for the p value of a test that the Beta = 2 (true beta)
test.data <- data.frame(Method = as.factor(names), test.stat = c(monte.carlo$test.ols, monte.carlo$test.prais, monte.carlo$test.ldv))

ggplot(test.data, aes(x=Method, y=test.stat, fill=Method)) + geom_boxplot(alpha=0.3) + guides(fill=FALSE) + theme_bw()

# Density plot of the Betas
ggplot(test.data, aes(x=test.stat, colour=Method) ) + geom_density() + theme_bw()


# here are the S.Es
se.data <- data.frame(Method = as.factor(names), se.stat = c(monte.carlo$se.ols, monte.carlo$se.prais, monte.carlo$se.ldv))

ggplot(se.data, aes(x=Method, y=se.stat, fill=Method)) + geom_boxplot(alpha=0.3) + guides(fill=FALSE) + theme_bw()

