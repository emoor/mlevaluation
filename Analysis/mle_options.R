################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file loads the required libraries and functions
#
################################################################################################

###################### Loading packages ###########################

library(foreign)
library(sandwich)
library(hdm)
library(matrixStats)
library(doSNOW)
library(xtable)
library(caret)
library(tidyverse)
library(MatchIt)
library(Zelig)
library(mvtnorm)
library(readstata13)
library(stringr)
library(glmnet)
library(ggplot2)
library(rdrop2)
library(stargazer)
library(lmtest)
library(pushoverr)
library(abind)
library(Matching)
library(randomForest)
library(gridExtra)
library(grid)
library(ranger)
library(gbm)
library(abind)
library(broom)
library(glmnetUtils)

###################### Load functions ###########################

source('f_lalonde_subset.R')
source('f_getEstimatorNames.R')
source('f_getData.R')
source('f_getLalondeData.R')
source('f_cia_estimators.R')
source('f_ps_cond_est.R')
source('f_saveResults.R')
source('f_sim_mc_diamond1.R')
source('f_sim_mc_busso.R')

# Function for foreach parallel
acomb <- function(...) abind(..., along=5)

MLEcomb <- function(...) {
  mapply(function(...) abind(..., along=5), ..., SIMPLIFY=FALSE)
}

###################### Get dropbox token ###########################

# Create new dropbox token if necessary, otherwise load the stored token
if (file.exists("dropbox_token_em.rds") == FALSE){
  
  dropbox_token <- drop_auth()
  saveRDS(dropbox_token, file = "dropbox_token.rds")
  
} else {
  
  dropbox_token <- readRDS("dropbox_token.rds")
  drop_acc(dtoken = dropbox_token)
  
}

###################### Set Pushover user and token ###########################

if (pushover_app.isset()==FALSE){set_pushover_app(token = "token")}
if (pushover_user.isset()==FALSE){set_pushover_user(user= "user")}


