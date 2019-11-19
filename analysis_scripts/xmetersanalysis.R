############################################################
# R code to analyze the Xthrow Var
# Kenyon College STAT 206 Data Analysis
# authors: J Katz, S Kim
############################################################


############################################################
## do preliminary work
## load packages
library(mosaic)
library(pwr)
library(agricolae)
library(multcomp)
library(car)
## you can use the citation() command by itself to cite R itself
## or citation("pkgname") to find the citation for a package
#this is a josh comment
############################################################
## load data
## attach a drive with letter T to your computer
## instructions and credentials are in the project log
discthrows <- read.csv("T:/Documents/STATS/datasets/discthrows.csv")
View(discthrows)
############################################################

############################################################
################Evaluating XMETERS!#########################
############################################################
## A side tangent on whether disc type affects Xmeters significantly
## could be used to evaluate accuracy

## one-way ANOVA
#xdiscmod <- lm(XMETERS ~ DISC, data = discthrows)
#xtypemod <- lm(XMETERS ~ TYPE, data = discthrows)
#(anova(xdiscmod))
#(anova(xtypemod))
## two-way ANOVA
xdifferencemod <- aov(XMETERS ~ DISC * TYPE, data = discthrows)
summary(xdifferencemod)
## there's no significant difference in variance within the groups
#summary(xdiscmod)
#summary(xtypemod)
## there IS a significant difference in different types of disc AND different throws, I think
#################################################
## CHECK THE CONDITIONS FOR THE ANOVA
#plot(xdiscmod, 1:2)
#plot(xtypemod, 1:2)
plot(xdifferencemod, 1:2)
## good nuff

############################################################
## run a Tukey comparison
#(HSD.test(xdiscmod, "DISC")) ## NEEDS LOOKED AT!
#(HSD.test(xtypemod, "TYPE")) ## ALSO NEEDS LOOKED AT!
## This creates our groups
(HSD.test(xdifferencemod, "DISC"))
(HSD.test(xdifferencemod, "TYPE"))

## run a Welch one-way ANOVA for unequal variances
res.aov.disc <- aov(XMETERS ~ DISC, data = discthrows)
res.aov.type <- aov(XMETERS ~ TYPE, data = discthrows)
summary(res.aov.disc)
summary(res.aov.type)

## run a multiple comparison
mcdiscanalysis <- glht(xdiscmod, linfct = mcp(DISC = "Tukey"))
(mcdiscsummary <- summary(mcdiscanalysis, test = adjusted("single-step")))

mctypeanalysis <- glht(xtypemod, linfct = mcp(TYPE = "Tukey"))
(mctypesummary <- summary(mctypeanalysis, test = adjusted("single-step")))
