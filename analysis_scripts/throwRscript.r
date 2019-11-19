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
plot(TOTALDIST ~ ThrowSN,
     main = "Distance by Throw Number",
     xlab = "Throw Serial Number",
     ylab = "Total Distance (meters)",
     data = discthrows)

############################################################
## Construct an interaction plot for between-term interaction effects
interaction.plot(discthrows$DISC,
                 discthrows$TYPE,
                 discthrows$TOTALDIST,
                 xlab = "Disc Type",
                 ylab = "Mean of Total Distance",
                 main = "Interaction Plot for Distance by Disc Number",
                 col=c(1:2))
interaction.plot(discthrows$TYPE,
                 discthrows$DISC,
                 discthrows$TOTALDIST,
                 xlab = "Throw Type",
                 ylab = "Mean of Total Distance",
                 main = "Interaction Plot for Distance by Throw Type",
                 col=c(1:3))

############################################################
## Construct a Diagnostic Plot for Unequal Variability
# calculate natural log of group means and sds
log.grp.means = log(mean(TOTALDIST ~ TYPE:DISC, data = discthrows))
log.grp.sd = log(sd(TOTALDIST ~ TYPE:DISC, data = discthrows))

# plot these values
xyplot(log.grp.sd~log.grp.means,
       main = "Diagnostic Plot for Unequal Variability",
       sub = "discthrows",
       type=c("p","r"))
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

## backwards elimination model building
full=lm(TOTALDIST ~ DISC + TYPE ,data = discthrows)
MSE=(summary(full)$sigma)^2
step(full, scale=MSE, direction="backward")
## forwards elimination
none=lm(TOTALDIST~1,data=discthrows)
step(none,scope=list(upper=full),scale=MSE,direction="forward")
## stepwise
none=lm(TOTALDIST~1,data=discthrows)
step(none,scope=list(upper=full),scale=MSE)
