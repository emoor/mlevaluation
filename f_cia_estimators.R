####################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This function estimates the average treatment effect (ATE/ATT) 
#              
#              Input:  ps_cond_data:     input data augmented with estimated propensity scores (e.g. Lasso_mdx)
#                                        and conditional outcome means (e.g. Lasso_myd1x and Lasso_myd0x)
#                      spec:             specification spec, a row from the specification grid spec_grid
#                      spec_list_all:    entire specification list
#                      oracleForms:      true functional forms of Y~X and D~X used to generate data
#                      propScore.grid:   propensity score methods grid
#                      condOutcome.grid: conditional outcome means methods grid
#                      trueEffect:       true causal effect
#
#              Output: list out containing the following arrays
#
#                      TEResultsCIA: treatment effect results array. For each treatment effect estimator (dim: length_estimator_list),
#                      for each propensity score method (dim: length_propensity_score_list), and for each conditional outcome
#                      means method (dim: length_conditional_outcome_means_list), the results array contains the estimated 
#                      treatment effect (Coef.), the standard error of the estimated treatment effect (SE), and the bias (Bias). The 
#                      dimension of the results array is therefore: 3*length_estimator_list*length_propensity_score_list*
#                      length_conditional_outcome_means_list.
#                      
#                      InfoResultsCIA: For each propensity score method (dim: length_propensity_score_list), and for each conditional outcome
#                      means method (dim: length_conditional_outcome_means_list), the info results array contains the true treatment
#                      effect (true.effect), the fraction of treated (fraction.treated), the mean propensity score of treated 
#                      (mean.ps.treated), the mean propensity score of control (mean.ps.control), the number of untrimmed
#                      observations (n.dx), the number of treated obs (n.yd1x), the number of control obs (n.yd0x), the number of
#                      trimmed obs (n.trim), the correlation of estimated propensity score with the true propensity score 
#                      (fs.ps.cor), the RMSE of the estimated propensity score (fs.ps.rmse), the correlation of estimated conditional
#                      outcome means with true conditional outcome means (fs.co1.cor and fs.co0.cor), the RMSE of the estimated
#                      conditional outcome means (fs.co1.rmse and fs.co0.rmse), and the estimated and true propensity score 
#                      histogram bin heights (hist.). 
#
#####################################################################################################

CIAEstimators <- function(ps_cond_data, spec, spec_list_all, oracleForms, propScore.grid, condOutcome.grid, trueEffect) {
  
  estimator_list <- spec_list_all$estimator_list_all
  
  # Initialize results matrix
  TEResultsCIA   <- array(NA, dim = c(3, length(spec_list_all$estimator_list_all), dim(propScore.grid)[1], dim(condOutcome.grid)[1]),
                       dimnames = list(c("Coef.", "SE", "Bias"), spec_list_all$estimator_list_all))
  
  InfoResultsCIA <- array(NA, dim = c((4*spec$psBins + 14), dim(propScore.grid)[1], dim(condOutcome.grid)[1]),
                       dimnames = list(c("true.effect", "fraction.treated", "mean.ps.treated", "mean.ps.control",
                                         "n.dx", "n.yd1x", "n.yd0x", "n.trim", "fs.ps.cor", "fs.ps.rmse",
                                         "fs.co1.cor", "fs.co0.cor", "fs.co1.rmse", "fs.co0.rmse",
                                         paste0("hist.", 1:spec$psBins, ".treated"), 
                                         paste0("hist.", 1:spec$psBins, ".control"),
                                         paste0("hist.", 1:spec$psBins, ".treated.true"), 
                                         paste0("hist.", 1:spec$psBins, ".control.true"))))
  
  # Loop over propScore.grid and condOutcome.grid
  for (ps in 1:dim(propScore.grid)[1]) {
    for (co in 1:dim(condOutcome.grid)[1]) {
      
      # Create mdx.string and myx.string (used to select the correct columns in ps_cond_data)
      if (propScore.grid[ps,"propScore"] == "Logit"){
        
        mdx.string <- paste0("Logit_", propScore.grid[ps,"variable_set_est"])
        
      } else if (propScore.grid[ps,"propScore"] == "RF"){
        
        mdx.string <- paste0("RF_tML", propScore.grid[ps,"tune_ML"], "_Dcl", propScore.grid[ps,"Dclassification"])
        
      } else if (propScore.grid[ps,"propScore"] == "Boost" | propScore.grid[ps,"propScore"] == "Lasso"){
        
        mdx.string <- paste0(propScore.grid[ps,"propScore"])
        
      }
      
      mdx.string <- paste0(mdx.string, "_mdx")

      if (condOutcome.grid[co,"condOutcome"] == "OLS"){
        
        myx.string <- paste0("OLS_", condOutcome.grid[co,"variable_set_est"])
        
      } else if (condOutcome.grid[co,"condOutcome"] == "RF"){
        
        myx.string <- paste0("RF_tML", propScore.grid[co,"tune_ML"])
        
      } else if (condOutcome.grid[co,"condOutcome"] == "Boost" | condOutcome.grid[co,"condOutcome"] == "Lasso"){
        
        myx.string <- paste0(condOutcome.grid[co,"condOutcome"])
        
      }
      
      myd1x.string <- paste0(myx.string, "_myd1x")
      myd0x.string <- paste0(myx.string, "_myd0x")
      
      # Select the correct columns in ps_cond_data 
      if (spec$param_of_interest=="ATE") {

        dataCIA <- ps_cond_data %>%
          dplyr::select(colnames(ps_cond_data)[!str_detect(colnames(ps_cond_data), "_m")],
                        m_dx = mdx.string, m_yd1x = myd1x.string, m_yd0x = myd0x.string)
        
      } else if (spec$param_of_interest=="ATT") {
        
        dataCIA <- ps_cond_data %>%
          dplyr::select(colnames(ps_cond_data)[!str_detect(colnames(ps_cond_data), "_m")],
                        m_dx = mdx.string, m_yd0x = myd0x.string)
        
      }
      
      # Get number of untrimmed observations
      n.obs.untrimmed <- dim(dataCIA)[1]
      
      # Trim propensity scores
      if (spec$trimPS != "0"){
        
        if (spec$trimPS == "0.01" | spec$trimPS == "0.05" | spec$trimPS == "0.1") {
          
          lowerBound <- as.numeric(as.character(spec$trimPS))
          upperBound <- 1-as.numeric(as.character(spec$trimPS))
          
        } else if (spec$trimPS == "MaxPS") {
          
          lowerBound <- min(subset(dataCIA, D==1)$m_dx)
          upperBound <- max(subset(dataCIA, D==0)$m_dx)
          
        } else if (spec$trimPS == "99QPS") {
          
          lowerBound <- quantile(subset(dataCIA, D==1)$m_dx, p = 0.01)
          upperBound <- quantile(subset(dataCIA, D==0)$m_dx, p = 0.99)
          
        } else if (spec$trimPS == "MaxPS001") {
          
          lowerBound.max <- min(subset(dataCIA, D==1)$m_dx)
          upperBound.max <- max(subset(dataCIA, D==0)$m_dx)
          lowerBound.001 <- 0.01
          upperBound.001 <- 0.99
          lowerBound <- max(lowerBound.max, lowerBound.001)
          upperBound <- min(upperBound.max, upperBound.001)
          
        }
        
        
        if (spec$param_of_interest == "ATE"){
          
          dataCIA <- dataCIA %>% dplyr::filter(m_dx >= lowerBound & m_dx <= upperBound)
          
        } else if (spec$param_of_interest == "ATT"){
          
          dataCIA <- dataCIA %>% dplyr::filter(m_dx <= upperBound)
          
        }
        
      } 
      
      # Define control vectors
      if (spec$datatype == "simulate_diamond1"){
        
        allVars <- c("X.X1", "X.X2", "X.X3", "X.X4", "X.X5", "X.X6", "X.X7", "X.X8", "X.X9", "X.X10")
        contVars <- c("X.X2", "X.X4","X.X7" ,"X.X10")
        dummyVars <- c("X.X1", "X.X3", "X.X5", "X.X6", "X.X8", "X.X9")
        
        form_DX_oracle <- as.formula(paste0("D ~ ", as.character(oracleForms$form_D)[3]))
        form_YX_oracle <- as.formula(paste0("Y ~ ", as.character(oracleForms$form_Y)[3]))
        
      } else if (spec$datatype == "real_nsw"){
        
        # Controls
        allVars <- c("age", "education", "black", "hispanic", "married", 
                     "nodegree", "re74", "re75", "u74", "u75")
        contVars <- c("age", "education","re74" ,"re75")
        dummyVars <- c("black", "hispanic", "married", "nodegree", "u74", "u75")
        
      } else if (spec$datatype == "simulate_busso"){
        
        # Controls
        allVars <- c("age", "education", "married", 
                     "nodegree", "re74", "re75", "u74", "u75")
        contVars <- c("age", "education","re74" ,"re75")
        dummyVars <- c("married", "nodegree", "u74", "u75")
        
      }
      
      x <- paste(allVars, collapse = "+")
      x2 <- paste(paste0("I(", contVars,"*", contVars ,")"), collapse = "+")
      xinter <- paste0("(", paste(allVars, collapse = "+"), ")^2")
      xl <- paste(x2, xinter, sep = " + ")
      
      # Initialize results matrix
      results <- matrix(NA, nrow = 3, ncol = length(estimator_list), dimnames = list(c("Coef", "SE", "Bias"), estimator_list))
      
      #############################################################################
      # Estimate mean comparison (MeanComp): no difference between ATE and ATT
      if ("MeanComp" %in% estimator_list){
        
        fit.mean <- lm(Y~D, data=dataCIA)
        results[1, which(estimator_list %in% "MeanComp")] <- coeftest(fit.mean, vcov = vcovHC(fit.mean, "HC3"))["D", "Estimate"]
        results[2, which(estimator_list %in% "MeanComp")] <- coeftest(fit.mean, vcov = vcovHC(fit.mean, "HC3"))["D", "Std. Error"]
        
      }
      
      #############################################################################
      # Estimate OLS Linear Regression with constant TE (SimpleOLS), Y on D and X 
      # (does usually not give ATE/ATT -> but often done): constant TE, no difference between ATE and ATT
      if ("SimpleOLS" %in% estimator_list){
        
        formYDX <- as.formula(paste("Y~D+",paste(x, collapse = "+"))) 
        fit.lm.short <- lm(formYDX, data=dataCIA)
        
        results[1, which(estimator_list %in% "SimpleOLS")] <- coeftest(fit.lm.short, vcov = vcovHC(fit.lm.short, "HC3"))["D", "Estimate"]
        results[2, which(estimator_list %in% "SimpleOLS")] <- coeftest(fit.lm.short, vcov = vcovHC(fit.lm.short, "HC3"))["D", "Std. Error"]
        
      }
      
      #############################################################################
      # Estimate Cond. Mean Regressions: non-constant TE, difference between ATE and ATT
      if ("Regression" %in% estimator_list){
        
        if (spec$param_of_interest=="ATE"){
          te.reg <- dataCIA[["m_yd1x"]] - dataCIA[["m_yd0x"]]
        }
        
        if (spec$param_of_interest=="ATT"){
          data.treated <- dataCIA %>% dplyr::filter(D==1)
          te.reg <- data.treated[["Y"]]-data.treated[["m_yd0x"]]
        }
        
        results[1, which(estimator_list %in% "Regression")] <- mean(te.reg)
        results[2, which(estimator_list %in% "Regression")] <- sd(te.reg)/sqrt(length(te.reg))
        
      }
      
      #############################################################################
      # Estimate Inverse Probability Weighting (IPW)
      if ("IPW" %in% estimator_list){
        
        if (spec$param_of_interest=="ATE"){
          
          te.ipw <- ((dataCIA[["Y"]]*dataCIA[["D"]])/dataCIA[["m_dx"]]) - 
            ((dataCIA[["Y"]]*(1-dataCIA[["D"]]))/(1-dataCIA[["m_dx"]]))
          
        }
        
        if (spec$param_of_interest=="ATT"){
          
          te.ipw <- ((dataCIA[["Y"]]*dataCIA[["D"]])/(mean(dataCIA[["D"]]))) - 
            ((dataCIA[["Y"]]*(1-dataCIA[["D"]])*dataCIA[["m_dx"]])/((1-dataCIA[["m_dx"]])*mean(dataCIA[["D"]])))
          
        }
        
        results[1, which(estimator_list %in% "IPW")] <- mean(te.ipw)
        results[2, which(estimator_list %in% "IPW")] <- sd(te.ipw)/sqrt(length(te.ipw))
        
      }
      
      #############################################################################
      # Prepare matching dataset for matching estimators
      if ("1NNMwR" %in% estimator_list | "BC1NNMwR" %in% estimator_list) {
        
        data.matching <- dataCIA
        
        if (spec$param_of_interest=="ATE"){
          Matc <- Match(Y = data.matching$Y, Tr = data.matching$D, X = data.matching$m_dx, estimand = "ATE", replace = T, ties = F)
          data.matching[,"Y0"] <- data.matching[Matc$index.control, "Y"]
          data.matching[,"Y1"] <- data.matching[Matc$index.treated, "Y"]

          data.matching[,"BCY0"] <- data.matching[Matc$index.control, "Y"] + 
            (data.matching[,"D"])*(data.matching[, "m_yd0x"] - data.matching[Matc$index.control, "m_yd0x"])
          data.matching[,"BCY1"] <- data.matching[Matc$index.treated, "Y"] + 
            (1-data.matching[,"D"])*(data.matching[, "m_yd1x"] - data.matching[Matc$index.treated, "m_yd1x"])
        }
        
        if (spec$param_of_interest=="ATT"){
          Matc <- Match(Y = data.matching$Y, Tr = data.matching$D, X = data.matching$m_dx, estimand = "ATT", replace = T, ties = F)
          index.treat <- which(data.matching[,"D"] == 1)
          data.matching[index.treat,"Y0"] <- data.matching[Matc$index.control, "Y"]
          
          data.matching[index.treat,"BCY0"] <- data.matching[Matc$index.control, "Y"] + 
            data.matching[index.treat,"m_yd0x"] - data.matching[Matc$index.control, "m_yd0x"]
        }
        
      }
      
      #############################################################################
      # Estimate 1NN Matching on the propensity score with replacement (1NNMwR)
      if ("1NNMwR" %in% estimator_list){
        
        if (spec$param_of_interest=="ATE"){
          te.1nnwM <- data.matching[,"Y1"] - data.matching[,"Y0"]
        }
        
        if (spec$param_of_interest=="ATT"){
          data.matching.treated <- data.matching %>% dplyr::filter(D==1)
          te.1nnwM <- data.matching.treated[["Y"]]-data.matching.treated[["Y0"]]
        }
        
        results[1, which(estimator_list %in% "1NNMwR")] <- mean(te.1nnwM)
        results[2, which(estimator_list %in% "1NNMwR")] <- sd(te.1nnwM)/sqrt(length(te.1nnwM))

      }

        #############################################################################
        # Estimate Hybrid Matching and Regression (Bias-corrected Matching), 1NN with replacement (BC1NNMwR)
        if ("BC1NNMwR" %in% estimator_list){

          if (spec$param_of_interest=="ATE"){
            te.bc1nnwM <- data.matching[,"BCY1"] - data.matching[,"BCY0"]
          }

          if (spec$param_of_interest=="ATT"){
            data.matching.treated <- data.matching %>% dplyr::filter(D==1)
            te.bc1nnwM <- data.matching.treated[["Y"]]-data.matching.treated[["BCY0"]]
          }

          results[1, which(estimator_list %in% "BC1NNMwR")] <- mean(te.bc1nnwM)
          results[2, which(estimator_list %in% "BC1NNMwR")] <- sd(te.bc1nnwM)/sqrt(length(te.bc1nnwM))

        }

      #############################################################################
      # Double Machine Learning (DoubleML)
      if ("DoubleML" %in% estimator_list){

        if (spec$param_of_interest=="ATE"){
           te.dml <- ((dataCIA[["D"]] * (dataCIA[["Y"]] - dataCIA[["m_yd1x"]])) / dataCIA[["m_dx"]]) -
                 (((1 - dataCIA[["D"]]) * (dataCIA[["Y"]] - dataCIA[["m_yd0x"]])) / (1 - dataCIA[["m_dx"]])) +
             dataCIA[["m_yd1x"]] - dataCIA[["m_yd0x"]]

        }
        
        if (spec$param_of_interest=="ATT"){
          te.dml <- (((dataCIA[["Y"]]-dataCIA[["m_yd0x"]])*dataCIA[["D"]])/mean(dataCIA[["D"]])) -
                 (((dataCIA[["Y"]]-dataCIA[["m_yd0x"]])*(1-dataCIA[["D"]])*dataCIA[["m_dx"]])/((1-dataCIA[["m_dx"]])*mean(dataCIA[["D"]])))

        }

        results[1, which(estimator_list %in% "DoubleML")] <- mean(te.dml)
        results[2, which(estimator_list %in% "DoubleML")] <- sd(te.dml)/sqrt(length(te.dml))

      }

      #############################################################################

      # Prepare TEresults and InfoResults
      results[3,] <- results[1,] - trueEffect # Bias
      dataCIA.treated <- dataCIA %>% dplyr::filter(D==1)
      dataCIA.control <- dataCIA %>% dplyr::filter(D==0)
      ps.density.treated <- hist(dataCIA.treated[["m_dx"]], breaks=seq(0,1,(1/spec$psBins)))$density
      ps.density.control <- hist(dataCIA.control[["m_dx"]], breaks=seq(0,1,(1/spec$psBins)))$density
      
      if (spec$datatype != "real_nsw"){
        ps.density.treated.true <- hist(dataCIA.treated[["truePS"]], breaks=seq(0,1,(1/spec$psBins)))$density
        ps.density.control.true <- hist(dataCIA.control[["truePS"]], breaks=seq(0,1,(1/spec$psBins)))$density
      }

      # Plugin TEresults and InfoResults
      TEResultsCIA[,,ps,co] <- results
      InfoResultsCIA["true.effect",ps,co] <- trueEffect
      InfoResultsCIA["fraction.treated",ps,co] <- dim(dataCIA.treated)[1]/dim(dataCIA)[1]
      InfoResultsCIA["mean.ps.treated",ps,co] <- mean(dataCIA.treated[["m_dx"]])
      InfoResultsCIA["mean.ps.control",ps,co] <- mean(dataCIA.control[["m_dx"]])
      InfoResultsCIA["n.dx",ps,co]   <- n.obs.untrimmed
      InfoResultsCIA["n.yd1x",ps,co] <- dim(dataCIA.treated)[1]
      InfoResultsCIA["n.yd0x",ps,co] <- dim(dataCIA.control)[1]
      InfoResultsCIA["n.trim",ps,co] <- dim(dataCIA)[1]
      
      if (spec$datatype != "real_nsw"){
        
        InfoResultsCIA["fs.ps.cor",ps,co]  <- cor(dataCIA[["truePS"]], dataCIA[["m_dx"]])
        InfoResultsCIA["fs.ps.rmse",ps,co] <- sqrt(mean((dataCIA[["truePS"]]-dataCIA[["m_dx"]])^2))
      
      
        if (spec$param_of_interest=="ATE"){
          InfoResultsCIA["fs.co1.cor",ps,co] <- cor(dataCIA.control[["Y1"]], dataCIA.control[["m_yd1x"]])
          InfoResultsCIA["fs.co1.rmse",ps,co] <- sqrt(mean((dataCIA.control[["Y1"]]-dataCIA.control[["m_yd1x"]])^2))
        } else if (spec$param_of_interest=="ATT"){
          InfoResultsCIA["fs.co1.cor",ps,co] <- NA
          InfoResultsCIA["fs.co1.rmse",ps,co] <- NA
        }
      
        InfoResultsCIA["fs.co0.cor",ps,co] <- cor(dataCIA.treated[["Y0"]], dataCIA.treated[["m_yd0x"]])
        InfoResultsCIA["fs.co0.rmse",ps,co] <- sqrt(mean((dataCIA.treated[["Y0"]]-dataCIA.treated[["m_yd0x"]])^2))
      
      }
      
      InfoResultsCIA[paste0("hist.", 1:spec$psBins, ".treated"),ps,co] <- ps.density.treated
      InfoResultsCIA[paste0("hist.", 1:spec$psBins, ".control"),ps,co] <- ps.density.control
      
      if (spec$datatype != "real_nsw"){
        
        InfoResultsCIA[paste0("hist.", 1:spec$psBins, ".treated.true"),ps,co] <- ps.density.treated.true
        InfoResultsCIA[paste0("hist.", 1:spec$psBins, ".control.true"),ps,co] <- ps.density.control.true
        
      }

      # Outlier detection and printing to log file
      outlier_ind <- sapply(estimator_list, function(x) abs(results[1,x])>1000)
      if (sum(outlier_ind, na.rm = T)!=0){
        sink("log_mle_outliers.txt", append=TRUE)
        cat("\n")
        cat("Outlier in: ", estimator_list[outlier_ind])
        cat("\n")
        cat("TE: ", results[1,outlier_ind])
        cat("\n")
        cat("Summary Y:")
        cat("\n")
        print(summary(dataCIA[["Y"]]))
        cat("Summary D:")
        cat("\n")
        print(summary(dataCIA[["D"]]))
        cat("Summary m_dx:")
        cat("\n")
        print(summary(dataCIA[["m_dx"]]))
        cat("Summary m_yd0x:")
        cat("\n")
        print(summary(dataCIA[["m_yd0x"]]))
        sink()
      }

    } # end of loop over condOutcome.grid
  } # end of loop over propScore.grid

out <- list(TEResultsCIA = TEResultsCIA, InfoResultsCIA = InfoResultsCIA)
  
return(out)   
}
