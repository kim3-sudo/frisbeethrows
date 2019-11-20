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

#EDA on xmeters
favstats(XMETERS~TYPE, data = discthrows) #SD's are a little odd, let's run a diagnostic plot
favstats(XMETERS~DISC, data = discthrows)

#Diagnostic plot
log.grp.means = log(mean(XMETERS ~ TYPE:DISC, data = discthrows))
log.grp.sd = log(sd(XMETERS ~ TYPE:DISC, data = discthrows))
xyplot(log.grp.sd~log.grp.means,type = c('p','r'))
trnsline = lm(log.grp.sd~log.grp.means)
summary(trnsline) #slope estimate is 0.645, let's try a transformation y^1-0.645
newXMETERS = (discthrows$XMETERS) ^ (1-trnsline$coefficients[2])

#New EDA
favstats(newXMETERS~TYPE, data = discthrows) 
favstats(newXMETERS~DISC, data = discthrows)

#variance looks a lot better, let's proceed using this transformation

#check for interaction
interaction.plot(discthrows$DISC,discthrows$TYPE, newXMETERS, col = 1:30) #yeah it's there

#build model
transformedmodel = aov(newXMETERS~discthrows$DISC*discthrows$TYPE)
summary(transformedmodel) #Interaction is still significant, so proceed investigating that

# But first, check conditions
plot(transformedmodel,1:2) #they're not met, arguably worse than original, so let's not use this model

#Thought: 0.635 is close-ish to 0.5, let's run a square root transformation
rootXMETERS = sqrt(discthrows$XMETERS)

#Second New EDA
favstats(rootXMETERS~TYPE, data = discthrows) 
favstats(rootXMETERS~DISC, data = discthrows)

#Looks good, let's check for interaction again
interaction.plot(discthrows$DISC,discthrows$TYPE, rootXMETERS, col = 1:30) #still there

#build the model
rootmodel = aov(rootXMETERS~DISC*TYPE, data = discthrows)
summary(rootmodel) #interaction still signficant

#check conditions
plot(rootmodel, 1:2) #yeah they're still bad, no logical transformation is fixing this, we'll proceed with the below analysis of the original XMETERS data

############################################################
################Evaluating XMETERS!#########################
############################################################
## A side tangent on whether disc type affects Xmeters significantly
## could be used to evaluate accuracy

## one-way ANOVA
xdiscmod <- lm(XMETERS ~ DISC, data = discthrows)
xtypemod <- lm(XMETERS ~ TYPE, data = discthrows)
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

## Interaction term was significant in the 2 Way ANOVA, so we should investigate that
interaction.plot(discthrows$DISC,discthrows$TYPE,discthrows$XMETERS, col = 1:30) #clear interaction present
interaction.plot(discthrows$TYPE,discthrows$DISC,discthrows$XMETERS, col = 1:30) #same thing, just plotted the other way. still very obvious
TukeyHSD(xdifferencemod, "DISC:TYPE") #disc flicks have more horizontal displacemenent
