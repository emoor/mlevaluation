#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This is the main file calling all analysis files
#
#########################################################################################################

# Clear memory
rm(list = ls())

# Set seed
set.seed(1989)

# Create cluster for parallelization
library(doParallel)
library(doRNG)
cores_used <- 1
use_parallel_computing <- T
if (use_parallel_computing == T) {
  writeLines(c(""), "log.txt")
  cores_used <- floor(0.45*parallel::detectCores())
  cl  <- makeCluster(cores_used, outfile = 'log.txt')
  registerDoParallel(cl)
} else {
  registerDoSEQ()
}

# Starting time of program
starttime.prog <- Sys.time()

# Set working directory
setwd("U:/Research/ML Evaluation/R_MLE")

# Load packages and functions, set options
source('mle_options.R')

# Clear log file
writeLines(c(""), "log_mle_iterations.txt")
writeLines(c(""), "log_mle_outliers.txt")
writeLines(c(""), "log_mle_fitted.txt")

############################################################################################
# General specification I  #################################################################
############################################################################################

# Parameter of interest
param_of_interest <- c("ATE") # "ATT" or "ATE", only for interactive model

# Number of overall replications
reps <- 5000 

# Number of repeated sample splittings (1: no repeated sample splitting)
sampleSplits <- c(1, 10) # 1, 10 

# Number of cross-fitting folds? (1: no cross-fitting)
crossFitFolds <- c(1, 2, 5, 10) # 1, 2, 5, 10

# Trim Propensity Scores?
trimPS <- c("MaxPS001") # "0.01" (or "0.05", "0.1"): Trim dataset before estimation to 
                        #   observations with propensity score 0.01 < ps < 0.99
                        # "MaxPS" ("99QPS"): Trim treated obs with PS larger than max 
                        #   PS (99%-quantile) of untreated obs (ATE: trim also obs with PS 
                        #   smaller than min PS (1%-quantile) of treated obs)) 
                        # "MaxPS001": trim obs that would be trimmed under MaxPS or 0.01

# Number of propensity score bins in graph
psBins <- c(20) # if not equal to 20, generate new results-dataset!

# Datatype: simulated or real data
datatype <- "simulate_diamond1" # "real_nsw", "simulate_diamond1", "simulate_busso", 

############################################################################################
# Choose Treatment Effect Estimator   ######################################################
############################################################################################

# Regression Estimator                 : Regression
# Inverse Probability Weighting        : IPW
# 1NN Matching w/ replacement          : 1NNMwR
# Bias-corrected 1NN Matching w/ rep.  : BC1NNMwR
# Double ML                            : DoubleML
# Simple OLS                           : SimpleOLS
# Simple Mean (Treat/Cont.) comparison : MeanComp

estimator_list_all <- c("MeanComp", "SimpleOLS", "Regression", "IPW", "1NNMwR", "BC1NNMwR", "DoubleML")

############################################################################################
# Choose Methods to Estimate Propensity Score and Conditional Outcome Means   ##############
############################################################################################

# "Simple OLS"    : "OLS" 
# "Logit"         : "Logit" 
# "Random Forest" : "RF"
# "Elastic Net"   : "Lasso"

propScore_methods_list   <- c("Logit", "RF", "Lasso") # "Logit", "RF", "Lasso" 
condOutcome_methods_list <- c("OLS", "RF", "Lasso") # "OLS", "RF", "Lasso"

############################################################################################
# General specification II  ################################################################
############################################################################################

# Choose variable set which is used to estimate propensity score and cond. outcomes
variable_set_est <- c("Main", "Oracle") # "Main": contains only main effects
                                        # "Oracle": for OLS/Logit, knows the variables which and 
                                        #   interactions/squared terms used in the DGP
                                        # "Full": main effects + all 2nd order interactions and 
                                        #   squared terms  

# Check that oracle is not included in real_nsw
if ("Oracle" %in% variable_set_est & datatype != "simulate_diamond1") warning("For empirical simulation study and real Lalonde datasets, Oracle variable set is not available!")

# Choose whether ML methods should be tuned with Caret
tune_ML <- c(0, 1) # 1: Yes, tune ML with caret, 0: No, use default specification

# Estimate D~X as classification or regression?
Dclassification <- c(0) # 1: Estimate D~X as classification, 0: Estimate D~X as regression

# Get grid of combinations for propScore_methods_list, condOutcome_methods_list, variable_set_est, tune_ML, Dclassification
propScore.grid <- expand.grid(propScore = propScore_methods_list, variable_set_est = variable_set_est, 
                              tune_ML = tune_ML, Dclassification = Dclassification)
propScore.grid[which(propScore.grid$propScore == "Logit"),c("tune_ML", "Dclassification")] <- NA
propScore.grid[which(propScore.grid$propScore == "RF"), c("variable_set_est")] <- NA
propScore.grid[which(propScore.grid$propScore == "Lasso"), c("tune_ML", "Dclassification", "variable_set_est")] <- NA
propScore.grid <- unique(propScore.grid)

condOutcome.grid <- expand.grid(condOutcome = condOutcome_methods_list, variable_set_est = variable_set_est, tune_ML = tune_ML)
condOutcome.grid[which(condOutcome.grid$condOutcome == "OLS"), c("tune_ML")] <- NA
condOutcome.grid[which(condOutcome.grid$condOutcome == "RF"), c("variable_set_est")] <- NA
condOutcome.grid[which(condOutcome.grid$condOutcome == "Lasso"),c("tune_ML", "variable_set_est")] <- NA
condOutcome.grid <- unique(condOutcome.grid)

# Create list with all general specification
general_spec_list <- list(param_of_interest = param_of_interest, reps = reps, sampleSplits = sampleSplits,
                          crossFitFolds = crossFitFolds, datatype = datatype, estimator_list_all = estimator_list_all, 
                          variable_set_est = variable_set_est, trimPS = trimPS, psBins = psBins, tune_ML = tune_ML,
                          Dclassification = Dclassification, propScore_methods_list = propScore_methods_list, 
                          condOutcome_methods_list = condOutcome_methods_list)

############################################################################################
# Dataset-specific specification  ##########################################################
############################################################################################

if (datatype == "real_nsw"){
  
  nsw_gender   <- c("men")      # c("men", "women") - > women only psid1/psid2 controls available
  comp_group   <- c("psid1")    # c("nsw", "psid1", "psid2", "psid3", "cps1", "cps2", "cps3")
  exper_group  <- c("control")  # c("treat", "control"): use experimental treatment or control group
                                    # treat: 1 = received treatment, control: 1 = experimental control group
  nsw_sample   <- c("Dehejia")  # c("Lalonde", "Dehejia", "SmithTodd") 
  draw_bs      <- c(1)          # 1: Draw bootstrap sample from original data, 0: Always take original sample
  trimmed_data <- c(0)          # 1: Use pre-trimmed data (same as in empirical simulation), 0: Use all data
  
  ds_specific_spec_list <- list(nsw_gender = nsw_gender, comp_group = comp_group, 
                                exper_group = exper_group, nsw_sample = nsw_sample, 
                                draw_bs = draw_bs, trimmed_data = trimmed_data)
  
} else if (datatype == "simulate_busso"){
  
  n_sim              <- c(1000) # c(200, 500, 1000, 2000, 5000)
  select_into_D      <- c(1)    # c(0.2, 0.5, 1, 2, 5) Tstar = treat_frac_cons + select_into_D*f(X): 
                                #  1: no change, >1: more selection / less overlap, <1: less selection / more overlap
  treat_frac_cons    <- c(0)    # Tstar = treat_frac_cons + select_into_D*f(X): 0: no change, >0: more treated, <0: less treated
  Ynoise             <- c(1)    # 0: no additinal normally distributed noise in Y1/Y0, 1: additional noise, 2: strong additional noise 
  
  ds_specific_spec_list <- list(n_sim = n_sim, select_into_D = select_into_D, treat_frac_cons = treat_frac_cons,
                                Ynoise = Ynoise)
  
} else if (datatype == "simulate_diamond1"){
  
  scenarioY       <- c("YG") #  Linearity and Additivity Y~X: c("YA", "YB", "YC", "YD", "YE", "YF", "YG", "YH", "YI")
  scenarioD       <- c("DG") #  Linearity and Additivity D~X: c("DA", "DB", "DC", "DD", "DE", "DF", "DG", "DH", "DI")
  n_sim           <- c(1000)
  select_into_D   <- c(1)    # Tstar = treat_frac_cons + select_into_D*f(X): 1: no change, 
                             #   >1: more selection / less overlap, <1: less selection / more overlap
  treat_frac_cons <- c(0)    # Tstar = treat_frac_cons + select_into_D*f(X): 0: no change, >0: more treated, <0: less treated
  Ynoise          <- c(1)    # 0: no additinal normally distributed noise in Y1/Y0, 1: moderate additional noise, 2: strong additional noise 
  
  ds_specific_spec_list <- list(n_sim = n_sim, scenarioY = scenarioY, scenarioD = scenarioD, 
                                select_into_D = select_into_D, treat_frac_cons = treat_frac_cons, Ynoise = Ynoise)
  
}  


######################################################################################################################
# Estimation  ########################################################################################################
######################################################################################################################

# Create specification grid (estimation will loop over this grid)
spec_list_all <- c(general_spec_list, ds_specific_spec_list)
spec_list     <- spec_list_all[ - which(names(spec_list_all) == "estimator_list_all" |
                                        names(spec_list_all) == "performance_measures" |
                                        names(spec_list_all) == "condOutcome_methods_list" |
                                        names(spec_list_all) == "propScore_methods_list" |
                                        names(spec_list_all) == "variable_set_est"|
                                        names(spec_list_all) == "tune_ML"|
                                        names(spec_list_all) == "Dclassification")]

spec_grid <- expand.grid(spec_list)

for (speci in 1:(dim(spec_grid)[1])){ # loop over specifications
  
  spec <- spec_grid[speci,] 
  
  cat(paste("Starting Specification ",speci,"/",dim(spec_grid)[1], "\n"))
  print(spec)
  
  set.seed(42)
  
  # DO  (sequantial)
  #results_reps <- foreach(repli=1:spec$reps, .combine='MLEcomb', .multicombine=TRUE, .inorder=FALSE, .packages=c('caret', 'gbm', 'randomForest', 'sandwich', 'rpart','glmnet','tidyverse', 'mvtnorm', 'matrixStats', 'lmtest', 'abind', 'Matching', 'readstata13', 'foreign')) %do% {
  # DO PAR (parallel)
  #results_reps <- foreach(repli=1:spec$reps, .combine='MLEcomb', .multicombine=TRUE, .inorder=FALSE, .packages=c('caret', 'gbm', 'randomForest', 'sandwich', 'rpart','glmnet','tidyverse', 'mvtnorm', 'matrixStats', 'lmtest', 'abind', 'Matching', 'readstata13', 'foreign')) %dopar% {
  # DO RNG (parallel)
  results_reps <- foreach(repli=1:spec$reps, .combine='MLEcomb', .multicombine=TRUE, .inorder=FALSE, .packages=c('caret', 'gbm', 'randomForest', 'sandwich', 'rpart','glmnet','tidyverse', 'mvtnorm', 'matrixStats', 'lmtest', 'abind', 'Matching', 'readstata13', 'foreign')) %dorng% {
    
    Sys.sleep(runif(1, min=0.2, max=10))
    sink("log_mle_iterations.txt", append=TRUE)
    cat(paste("Starting iteration",repli,"/",reps, "\n"))
    sink()

    #############################################################################
    # Load Dataset   ############################################################
    #############################################################################
    
    out <- getData(spec)

    data          <- out$data
    trueEffect    <- out$trueEffect
    oracleForms   <- out$oracleForms
    
    if (mean(data$D)==0 | mean(data$D)==1) {stop("Only treated or untreated in dataset!")}

    #############################################################################
    # Run main analysis   #######################################################
    #############################################################################

    # Define how many repeated sample splits (nb_rss)
    if (spec$crossFitFolds == 1){
      nb_rss <- 1
    } else {
      nb_rss <- spec$sampleSplits
    }

    # Container for results from different sample splits
    TEResults   <- array(NA, dim = c(3, length(spec_list_all$estimator_list_all), dim(propScore.grid)[1], 
                                     dim(condOutcome.grid)[1], nb_rss),
                         dimnames = list(c("Coef.", "SE", "Bias"), spec_list_all$estimator_list_all))
    InfoResults <- array(NA, dim = c((4*spec$psBins + 14), dim(propScore.grid)[1], dim(condOutcome.grid)[1], nb_rss),
                         dimnames = list(c("true.effect", "fraction.treated", "mean.ps.treated", "mean.ps.control",
                                            "n.dx", "n.yd1x", "n.yd0x", "n.trim", "fs.ps.cor", "fs.ps.rmse",
                                            "fs.co1.cor", "fs.co0.cor", "fs.co1.rmse", "fs.co0.rmse", 
                                            paste0("hist.", 1:spec$psBins, ".treated"),
                                            paste0("hist.", 1:spec$psBins, ".control"),
                                            paste0("hist.", 1:spec$psBins, ".treated.true"), 
                                            paste0("hist.", 1:spec$psBins, ".control.true"))))
    
    # Repeat estimation for different sample splits
    for (rss in 1:nb_rss){
      
      ps_cond_data <- get_ps_cond_data(data, spec, spec_list_all, oracleForms)
      
      out <- CIAEstimators(ps_cond_data, spec, spec_list_all, oracleForms, propScore.grid, condOutcome.grid, trueEffect)

      TEResults[,,,,rss] <- out$TEResultsCIA
      InfoResults[,,,rss] <- out$InfoResultsCIA

    }

    # Combine results from different sample splits (take median)
    if (nb_rss == 1){
      
      TEResults2 <- TEResults[,,,,1]
      InfoResults2 <- InfoResults
      
    } else {
      
      TEResults2 <- apply(TEResults, 1:4, median)
      InfoResultsMedian <- apply(InfoResults, 1:3, median)
      InfoResultsNA <- InfoResultsMedian
      InfoResultsNA[,,] <- NA
      InfoResults2 <- abind(InfoResultsMedian,InfoResultsNA, along=4)
      
    }

    res <- list(TEResults2 = TEResults2, InfoResults2 = InfoResults2)
    res
  }
  

  #############################################################################
  # Post-processing and Tables   ##############################################
  #############################################################################
  saveResults(results_reps, spec_list_all, spec, estimator_list_all, propScore.grid, condOutcome.grid)
  
} # end over specification loop


#############################################################################
# End #######################################################################
#############################################################################

# End time
endtime.prog <- Sys.time()

# Stop cluster
if (use_parallel_computing) {
  stopCluster(cl)
}

# Get elapsed time
print(paste("Time start: ", starttime.prog))
print(paste("Time end: ", endtime.prog))
time_elapsed <- round(as.numeric(endtime.prog - starttime.prog, units = "hours"),2)

# Send pushover notification to smartphone
pushover(message=paste0("MLE code run finished. Time elapsed: ", time_elapsed, " hours."), title="R @ RDSX")

