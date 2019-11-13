############################################################
# R code to analyze the frisbee throws dataset
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

############################################################
## load data
## attach a drive with letter T to your computer
## instructions and credentials are in the project log
discthrows <- read.csv("T:/Documents/STATS/datasets/discthrows.csv")
View(discthrows)
############################################################

############################################################
## do some EDA
head(discthrows)
discmod <- lm(TOTALDIST ~ DISC, data = discthrows)
plot(discmod, c(1:2,4))
throwmod <- lm(TOTALDIST ~ TYPE, data = discthrows)
plot(throwmod, c(1:2,4))
boxplot(TOTALDIST ~ DISC,
        main = "Boxplot of Distance by Disc Type",
        xlab = "Disc Type",
        ylab = "Distance",
        data = discthrows)
boxplot(TOTALDIST ~ TYPE,
        main = "Boxplot of Distance by Throw Type",
        xlab = "Throw Type",
        ylab = "Distance",
        data = discthrows)
favstats(TOTALDIST ~ DISC,
        data = discthrows)[c("DISC","mean","sd","n")]
favstats(TOTALDIST ~ TYPE,
        data = discthrows)[c("TYPE","mean","sd","n")]
## see if the throw distance was affected by serial number
plot(TOTALDIST ~ factor(ThrowSN), data = discthrows)

############################################################
## Construct an interaction plot for between-term interaction effects
interaction.plot(discthrows$TOTALDIST,
                 discthrows$TYPENUM,
                 discthrows$DISCNUM,
                 col=c(1:2))
interaction.plot(discthrows$TOTALDIST,
                 discthrows$factor(DISCNUM),
                 discthrows$factor(TYPENUM),
                 col=c(1:3))

############################################################
## Construct a Diagnostic Plot for Unequal Variability
# calculate natural log of group means and sds
log.grp.means = log(mean(TOTALDIST ~ TYPE, data = discthrows))
log.grp.sd = log(sd(TOTALDIST ~ TYPE, data = discthrows))

# plot these values
xyplot(log.grp.sd~log.grp.means, type=c("p","r"))
(trnsline = lm(log.grp.means~log.grp.sd))

############################################################
## Construct an overall F-test for the model
fullmod <- aov(TOTALDIST~DISC*TYPE, data = discthrows)
nullmod <- aov(TOTALDIST~1, data = discthrows)
anova(fullmod, nullmod)

## Check the assumptions for a two-way ANOVA with interaction
plot(fullmod, 1:2)
leveneTest(discthrows$TOTALDIST, discthrows$DISC:discthrows$TYPE)
        ## a big P-val means we have homoscedasticity :)

############################################################
## Construct an F-test for two-way interactions
distancemod <- aov(TOTALDIST ~ DISC*TYPE, data = discthrows)
summary(distancemod)
## the interaction is NOT significant at 0.05 = alpha

############################################################
## Conduct an F-test for each main effect, since interaction is not significant
        ## DISC and TYPE are both significant!
        ## need the agricolae library
mean(TOTALDIST ~ DISC, data = discthrows)
mean(TOTALDIST ~ TYPE, data = discthrows)
plot(LSD.test(distancemod, "DISC", group = TRUE, p.adj = "none"))
plot(LSD.test(distancemod, "TYPE", group = TRUE, p.adj = "none"))

############################################################
############################################################
## MAKING THE MODEL
realmod <- aov(TOTALDIST ~ DISC + TYPE, data = discthrows)
summary(realmod)
reallm <- lm(TOTALDIST ~ DISC + TYPE, data = discthrows)
summary(reallm)

############################################################
################Evaluating XMETERS!#########################
############################################################
## A side tangent on whether disc type affects Xmeters significantly
## could be used to evaluate accuracy

## one-way ANOVA
xdiscmod <- lm(XMETERS ~ DISC, data = discthrows)
xtypemod <- lm(XMETERS ~ TYPE, data = discthrows)
(anova(xdiscmod))
(anova(xtypemod))
        ## there's no significant difference in variance within the groups
summary(xdiscmod)
summary(xtypemod)
        ## there IS a significant difference in different types of disc AND different throws, I think
plot(xdiscmod, 1:2)
plot(xtypemod, 1:2)
        ## good nuff

############################################################
## run a Tukey comparison
(HSD.test(xdiscmod, "DISC")) ## NEEDS LOOKED AT!
(HSD.test(xtypemod, "TYPE")) ## ALSO NEEDS LOOKED AT!

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
