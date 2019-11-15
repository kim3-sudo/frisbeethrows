############################################################
# R code to analyze the generate plots
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

plot(YMETERS ~ XMETERS,
     main = "Vertical Distance by Absolute Horizontal Displacement",
     xlab = "Absolute Horizontal Displacement (meters)",
     ylab = "Vertical Distance (meters)",
     data = discthrows)