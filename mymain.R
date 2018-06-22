#setwd("~/Documents/grad_school/school/classes/2016-2017_spring/stat542/code/project1/alternative_stats_resubmission")

#load libraries, though error if don't exist:
library(leaps)  # regsubsets
library(glmnet)  # glmnet for lasso and ridge
library(moments)  # skewness
library(corrplot)  # corrplot
library(pls)  
library(gbm)  #gbm for random forest

#rm(list=ls()) #remove all variables from workspace

source("Simple_Model_ZR.R")

source("Lasso_DZ.R")

source("GBM_JL.R")