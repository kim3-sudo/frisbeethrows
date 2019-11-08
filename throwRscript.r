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
## TEST READ FROM SERVER
Alcohol <- read.csv("T:/Documents/STATS/datasets/Alcohol.csv") # a test file
View(Alcohol) # view test file
## ACTUAL FILE
#frisbee <- read.csv("T:/Documents/STATS/datasets/frisbee.csv")
#View(frisbee)
#here's a new comment.