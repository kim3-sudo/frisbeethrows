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
library(asbio)
## you can use the citation() command by itself to cite R itself
## or citation("pkgname") to find the citation for a package

############################################################
## load data
## attach a drive with letter T to your computer
## instructions and credentials are in the project log
## TEST READ
Alcohol <- read.csv("T:/Documents/STATS/datasets/Alcohol.csv")
View(Alcohol)