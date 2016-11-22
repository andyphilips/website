#	Andy Philips
#	aphilips@pols.tamu.edu
#	Texas A&M University
#	4/20/15
#
#
#	An introduction to graphics in R using ggplot2
# -----------------------------------------------------#

# R has very flexible and relatively intuitive base graphics capabilities. However, there are times when one needs additional flexibility in making graphs. The package ggplot2 provides substantial flexibility and makes great-looking graphs (see lattice as another example of a good graphics package). Based off the "grammar of graphics", ggplot2 is designed to start simple---every graph has data, expressed as a geometry that is mapped onto a coordinate system. Add-ons to this (aesthetics) add complexity.

#install.packages("ggplot2", dependencies=TRUE)
#install.packages("RColorBrewer", dependencies=TRUE)
library(ggplot2)
library(RColorBrewer)
#	--------------------------------------------------#


#	--------------------------------------------------#
# Make up some cross-sectional data about corruption's effect on growth:
set.seed(20343)
corruption <- rnorm(150,6,2)
democracy <- as.numeric(rnorm(150)>0)
growth <- 20 - 5*log(corruption) + 5*democracy  + rnorm(150)
dataset <- data.frame(growth,corruption,democracy)
dataset$democracy[democracy==1] <- "democracy"
dataset$democracy[democracy==0] <- "non-democracy"
dataset # let's look at the data
#	--------------------------------------------------#


#	------------ scatterplot --------------------------#
# ggplot's command first specifies the dataset, then the aesthetics (x and y). The + specifies the type of plot you want. In this case, we want a simple scatterplot between corruption and growth
ggplot(data=dataset, aes(x=corruption,y=growth)) + geom_point()

# the section ggplot(data=dataset, aes(x=corruption,y=growth)) is the core component that identifies the data and variables of interest. The second part, + geom_point(), is a layer that we add to tell ggplot what to do. There are tons of layers, and (as the name suggests), we can use more than one layer.

# we can clean this up a bit by adding labels and a "theme". Other themes include theme_grey(), theme_minimal(), theme_classic(), and theme_bw().

ggplot(data=dataset, aes(x=corruption,y=growth)) +
	geom_point(color="blue") + # color the data
	labs(x="Level of Corruption", y = "Growth Rate", title= "Corruption's Effect on Growth") + # add labels
	theme_minimal() # Removes the grayscale

# We can differentiate between democracies and non-democracies.
ggplot(data=dataset, aes(x=corruption,y=growth)) +
	geom_point(aes(color=democracy)) +	# color the data by democracy
	labs(x="Level of Corruption", y = "Growth Rate", title= "Corruption's Effect on Growth") + # add labels
	theme_minimal() # Removes the grayscale

# Notice that outlier country with high growth? We can add a layer that labels it:
ggplot(data=dataset, aes(x=corruption,y=growth)) +
	geom_point(aes(color=democracy)) + # color the data by democracy
	labs(x="Level of Corruption", y = "Growth Rate", title= "Corruption's Effect on Growth") + # add labels
	theme_minimal() + # Removes the grayscale
	annotate("text", label="Rep. of High Growth", x=1.7, y=32, size=3, fontface="bold.italic") # add country label
#	--------------------------------------------------#


#	----------- lowess/line-plots ----------------------------#
# ggplot makes it easy to graph a lowess smoothed estimate of the relationship between corruption and growth:
ggplot(data=dataset, aes(x=corruption,y=growth)) +
	geom_smooth()	+	# lowess
	theme_minimal()	# Removes the grayscale

# We can also add a command to the lowess to differentiate between high and low democracies:
ggplot(data=dataset, aes(x=corruption,y=growth)) +
	geom_smooth(aes(color=democracy))	+	# lowess
	labs(x="Level of Corruption", y = "Growth Rate", title= "Corruption's Effect on Growth") + # add labels
	theme_minimal() # Removes the grayscale

# and add the underlying scatterplot to get a better sense of the data:
ggplot(data=dataset, aes(x=corruption,y=growth)) +
	geom_smooth(aes(color=democracy))	+	# lowess
	geom_point(aes(color=democracy)) + # add points
	labs(x="Level of Corruption", y = "Growth Rate", title= "Corruption's Effect on Growth") + # add labels
	theme_minimal()	# Removes the grayscale

# We can change both the color and the shape of the scatter points in the geom_point command:
ggplot(data=dataset, aes(x=corruption,y=growth)) +
	geom_smooth(aes(color=democracy))	+	# lowess
	geom_point(aes(color=democracy, shape=democracy)) +	# add points, change both color and shape
	labs(x="Level of Corruption", y = "Growth Rate", title= "Corruption's Effect on Growth") + # add labels
	theme_minimal()	# Removes the grayscale

# Instead, we could add a rug plot to keep the plot clearer
ggplot(data=dataset, aes(x=corruption,y=growth)) +
	geom_smooth(aes(color=democracy))	+	# lowess
	geom_rug(aes(color=democracy)) + # add rug
	labs(x="Level of Corruption", y = "Growth Rate", title= "Corruption's Effect on Growth") + # add labels
	theme_minimal()	# Removes the grayscale

# While a lowess provides a more localized fit than a regression line, we may just want to plot a straight best-fit line. This is done in the geom_smooth command:
ggplot(data=dataset, aes(x=corruption,y=growth)) +
	geom_smooth( aes(color=democracy), method="lm")	+	# lowess
	geom_rug(aes(color=democracy)) + # add rug
	labs(x="Level of Corruption", y = "Growth Rate", title= "Corruption's Effect on Growth") + # add labels
	theme_minimal()	# Removes the grayscale

# in addition, the legend title is redundant and could be removed. And let's place the legend up top:
ggplot(data=dataset, aes(x=corruption,y=growth)) +
	geom_smooth( aes(color=democracy), method="lm")	+	# lowess
	geom_rug(aes(color=democracy)) + # add rug
	labs(x="Level of Corruption", y = "Growth Rate", title= "Corruption's Effect on Growth") + # add labels
	theme_minimal()	+ # Removes the grayscale
	theme(legend.title=element_blank()) + # remove legend title
	theme(legend.position="top") # place legend up top

# by specifying se=FALSE in the geom_smooth() layer, we can remove the confidence intervals.
ggplot(data=dataset, aes(x=corruption,y=growth)) +
	geom_smooth( aes(color=democracy), method="lm", se=FALSE)	+	# lowess
	geom_rug(aes(color=democracy)) + # add rug
	labs(x="Level of Corruption", y = "Growth Rate", title= "Corruption's Effect on Growth") + # add labels
	theme_minimal()	+ # Removes the grayscale
	theme(legend.title=element_blank()) + # remove legend title
	theme(legend.position="top") # place legend up top
#	--------------------------------------------------#


#	----------- box-plots ----------------------------#
# Discrete X variables can be handled easily in ggplot. Here are some ways of visualizing this. We will use the dichtotomous democracy measure as the main focus. First a basic boxplot:
ggplot(data=dataset, aes(x=democracy,y=growth)) + geom_boxplot()

# We can make it look better:
ggplot(data=dataset, aes(x=democracy,y=growth)) +
	geom_boxplot(aes(color=democracy)) + # add color by democracy type
	theme_minimal()	# remove grayscale
#	--------------------------------------------------#


#	----------- violin-plots ----------------------------#
# Another recent plot that is gaining in popularity is the so-called violin plot:
ggplot(data=dataset, aes(x=democracy,y=growth)) +
	geom_violin(aes(color=democracy)) + # add color by democracy type
	theme_minimal()	# remove grayscale

# if we wanted to remove the legend entirely, we could do so using the following command:
ggplot(data=dataset, aes(x=democracy,y=growth)) +
	geom_violin(aes(color=democracy)) + # add color by democracy type
	theme_minimal() +
	theme(legend.position="none") # no legend

# we could also flip the axes:
ggplot(data=dataset, aes(x=democracy,y=growth)) +
	geom_violin(aes(color=democracy)) + # add color by democracy type
	coord_flip() + # flip axes
	theme_minimal() +
	theme(legend.position="none") # no legend

# this would look even better with points added (jittered to avoid overlap):
ggplot(data=dataset, aes(x=democracy,y=growth)) +
	geom_violin(aes(color=democracy)) + # add color by democracy type
	coord_flip() + # flip axes
	geom_jitter(aes(color=democracy), position= position_jitter(width=0.05)) + # add jittered points
	theme_minimal() +
	theme(legend.position="none") # no legend

# notice that the command for the legend must come after theme_minimal(), or else it will give you problems (not work).
#	--------------------------------------------------#


#	----------- splitting plots ----------------------------#
# sometimes we want 2+ plots side by side on one panel. This is done using the facet_wrap command. So if we wanted to plot a scatter plot of corruption and growth and separate by democracy:
ggplot(data=dataset, aes(x=corruption,y=growth)) +
	geom_point() +
	labs(x="Level of Corruption", y = "Growth Rate", title= "Corruption's Effect on Growth") + # add labels
	theme_minimal()	+
	facet_wrap(~democracy, nrow=1, scales="free")	# separate by democracy type

# Removing scales = "free" keeps them on a single axis. We will do that, add color (in the geom_point layer), place them on 2 rows, and remove the legend:
ggplot(data=dataset, aes(x=corruption,y=growth)) +
	geom_point(aes(color=democracy)) +
	labs(x="Level of Corruption", y = "Growth Rate", title= "Corruption's Effect on Growth") + # add labels
	theme_minimal()	+
	facet_wrap(~democracy, nrow=2) + # separate by democracy type, 1 row
	theme(legend.position="none") # remove legend
#	--------------------------------------------------#


#	----------- saving plots ----------------------------#
# Now that we have made a variety of plots, we want to save them. The ggsave command saves the last ggplot. We first generate the plot, then save it as a .png under the name "myplot":
ggplot(data=dataset, aes(x=democracy,y=growth)) +
	geom_violin(aes(color=democracy)) + # add color by democracy type
	theme_minimal()	# remove grayscale
ggsave("myplot.png", dpi=300, width=4.8, height=3)
# of course, if you're using something like RStudio, you can just export the plot using the export option
#	--------------------------------------------------#


#	----------- Color ----------------------------#
# Finally, we can change the color. Of couse we can manually alter colors:
ggplot(data=dataset, aes(x=corruption,y=growth)) +
	geom_point(aes(color=democracy)) + # color the data by democracy
	labs(x="Level of Corruption", y = "Growth Rate", title= "Corruption's Effect on Growth") + # add labels
	theme_minimal()	+ # Removes the grayscale
	scale_color_manual(values=c("chartreuse", "khaki1")) # change colors manually

# Also, Cynthia Brewer's RColorBrewer package provides great colors for a variety of applications. Since we have a qualitative, not sequential set of democracy/non-democracy, we can use Accent. But here are all the different styles:
display.brewer.all()

ggplot(data=dataset, aes(x=corruption,y=growth)) +
	geom_point(aes(color=democracy)) + # color the data by democracy
	labs(x="Level of Corruption", y = "Growth Rate", title= "Corruption's Effect on Growth") + # add labels
	theme_minimal()	+ # Removes the grayscale
	scale_color_brewer(palette="Accent")
#	--------------------------------------------------#


#	--------------------------------------------------#
# 	Some concluding thoughts for using ggplot:
#	1. Plan out what your x, y variables are, and what your potential aes variables will be
#	2. Start simple, move to complex. Each "+" addition changes a small part of your plot, and can be used for most types of plots.
#	3. Less chartjunk is better! Emphasize only the main message your figure is trying to convey.
#	4. Be aware that your audience may be colorblind or seeing your plot in B&W
#	5. This is by no means all that ggplot can do...there are lots of websites that have other applications.
#	--------------------------------------------------#
