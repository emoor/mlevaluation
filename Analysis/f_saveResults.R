####################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This function saves the results from all replications to .RData files (input for Shiny app)
#              
#              Input:  results_reps:     array with estimation results (TEResults2 and InfoResults2) from all replications
#                      spec_list_all:    entire specification list
#                      spec:             specification spec, a row from the specification grid spec_grid
#                      estimator_list_all:      true functional forms of Y~X and D~X used to generate data
#                      propScore.grid:   propensity score methods grid
#                      condOutcome.grid: conditional outcome means methods grid
#
#####################################################################################################

saveResults <- function(results_reps, spec_list_all, spec, estimator_list_all, propScore.grid, condOutcome.grid){
  
    if (length(estimator_list_all)==1){
      stop("error in saveResults, only one estimator in list")
    }
  
    if (spec$reps > 1){
    
      TEResults <- results_reps$TEResults2
      InfoResults <- results_reps$InfoResults2[,,,1,]
      
      critval90 <- qnorm(0.95)
      critval95 <- qnorm(0.975)
      
      res_Coverage_90 <- ((TEResults["Bias",,,,]-critval90*TEResults["SE",,,,])<0) & ((TEResults["Bias",,,,]+critval90*TEResults["SE",,,,])>0)
      res_Coverage_95 <- ((TEResults["Bias",,,,]-critval95*TEResults["SE",,,,])<0) & ((TEResults["Bias",,,,]+critval95*TEResults["SE",,,,])>0)
      
      # Aggregate results over replications
      res_median_coef      <- apply(TEResults["Coef.",,,,], c(1:3), median, na.rm = TRUE)
      median_coef_repe     <- array(rep(res_median_coef, spec$reps), dim = c(length(spec_list_all$estimator_list_all), 
                                    dim(propScore.grid)[1], dim(condOutcome.grid)[1], spec$reps))
        
      res_mean_coef        <- apply(TEResults["Coef.",,,,], c(1:3), mean, na.rm = TRUE)
      res_mean_bias        <- apply(TEResults["Bias",,,,], c(1:3), mean, na.rm = TRUE)
      res_median_bias      <- apply(TEResults["Bias",,,,], c(1:3), median, na.rm = TRUE)
      res_rmse             <- sqrt(apply((TEResults["Bias",,,,])^2, c(1:3), mean, na.rm = TRUE))
      res_rmse975          <- sqrt(apply((TEResults["Bias",,,,])^2, c(1:3), quantile, p = 0.975, na.rm = TRUE))
      res_rmse500          <- sqrt(apply((TEResults["Bias",,,,])^2, c(1:3), quantile, p = 0.5, na.rm = TRUE))
      res_rmse025          <- sqrt(apply((TEResults["Bias",,,,])^2, c(1:3), quantile, p = 0.025, na.rm = TRUE))
      res_rmse_rel         <- sqrt(apply((TEResults["Bias",,,,]/median_coef_repe)^2, c(1:3), mean, na.rm = TRUE))
      res_mae              <- apply(abs(TEResults["Bias",,,,]), c(1:3), mean, na.rm = TRUE)
      res_mae975           <- apply(abs(TEResults["Bias",,,,]), c(1:3), quantile, p = 0.975, na.rm = TRUE)
      res_mae500           <- apply(abs(TEResults["Bias",,,,]), c(1:3), quantile, p = 0.5, na.rm = TRUE)
      res_mae025           <- apply(abs(TEResults["Bias",,,,]), c(1:3), quantile, p = 0.025, na.rm = TRUE)
      res_mae_rel          <- apply(abs(TEResults["Bias",,,,]/median_coef_repe), c(1:3), mean, na.rm = TRUE)
      res_sd               <- apply(TEResults["Coef.",,,,], c(1:3), sd, na.rm = TRUE)*sqrt((spec$reps-1)/spec$reps)
      res_mean_se          <- apply(TEResults["SE",,,,], c(1:3), mean, na.rm = TRUE)
      res_CR_90            <- apply(res_Coverage_90, c(1:3), mean, na.rm = TRUE)
      res_CR_95            <- apply(res_Coverage_95, c(1:3), mean, na.rm = TRUE)
      
      res_true_effect      <- apply(InfoResults["true.effect",,,], c(1:2), mean, na.rm = TRUE)
      res_fraction_treated <- apply(InfoResults["fraction.treated",,,], c(1:2), mean, na.rm = TRUE)
      res_mean_ps_treated  <- apply(InfoResults["mean.ps.treated",,,], c(1:2), mean, na.rm = TRUE)
      res_mean_ps_control  <- apply(InfoResults["mean.ps.control",,,], c(1:2), mean, na.rm = TRUE)
      res_n_dx             <- apply(InfoResults["n.dx",,,], c(1:2), mean, na.rm = TRUE)
      res_n_yd1x           <- apply(InfoResults["n.yd1x",,,], c(1:2), mean, na.rm = TRUE)
      res_n_yd0x           <- apply(InfoResults["n.yd0x",,,], c(1:2), mean, na.rm = TRUE)
      res_n_trim           <- apply(InfoResults["n.trim",,,], c(1:2), mean, na.rm = TRUE)
      
      res_median_ps_hist_tr <- apply(InfoResults[paste0("hist.", 1:spec$psBins, ".treated"),,,], c(1:3), median, na.rm = TRUE)
      res_median_ps_hist_co <- apply(InfoResults[paste0("hist.", 1:spec$psBins, ".control"),,,], c(1:3), median, na.rm = TRUE)
      
      if (spec$datatype != "real_nsw"){
        
        res_fs.ps.cor        <- apply(InfoResults["fs.ps.cor",,,], c(1:2), mean, na.rm = TRUE)
        res_fs.ps.rmse       <- apply(InfoResults["fs.ps.rmse",,,], c(1:2), mean, na.rm = TRUE)
        res_fs.co1.cor       <- apply(InfoResults["fs.co1.cor",,,], c(1:2), mean, na.rm = TRUE)
        res_fs.co1.rmse      <- apply(InfoResults["fs.co1.rmse",,,], c(1:2), mean, na.rm = TRUE)
        res_fs.co0.cor       <- apply(InfoResults["fs.co0.cor",,,], c(1:2), mean, na.rm = TRUE)
        res_fs.co0.rmse      <- apply(InfoResults["fs.co0.rmse",,,], c(1:2), mean, na.rm = TRUE)
        
        res_median_ps_hist_tr_true <- apply(InfoResults[paste0("hist.", 1:spec$psBins, ".treated.true"),,,], c(1:3), median, na.rm = TRUE)
        res_median_ps_hist_co_true <- apply(InfoResults[paste0("hist.", 1:spec$psBins, ".control.true"),,,], c(1:3), median, na.rm = TRUE)
        
      }
      

      # Create final table
      final_table <- array(NA, dim = c(dim(res_mean_coef)[1], dim(res_mean_coef)[2], dim(res_mean_coef)[3], 17))
      final_table <- round(abind(res_mean_coef, res_mean_bias, res_median_bias, 
                                 res_rmse, res_rmse975, res_rmse500, res_rmse025, res_rmse_rel, res_mae, res_mae975, res_mae500, res_mae025,
                                 res_mae_rel, res_sd, res_mean_se, res_CR_95, res_CR_90, along = 4),5)
    
    } else if (spec$reps == 1){
      
      TEResults <- results_reps$TEResults2
      InfoResults <- results_reps$InfoResults2[,,,1]
      
      critval90 <- qnorm(0.95)
      critval95 <- qnorm(0.975)
      
      res_Coverage_90 <- ((TEResults["Bias",,,]-critval90*TEResults["SE",,,])<0) & ((TEResults["Bias",,,]+critval90*TEResults["SE",,,])>0)
      res_Coverage_95 <- ((TEResults["Bias",,,]-critval95*TEResults["SE",,,])<0) & ((TEResults["Bias",,,]+critval95*TEResults["SE",,,])>0)
      
      res_median_coef      <- TEResults["Coef.",,,]

      res_mean_coef        <- TEResults["Coef.",,,]
      res_mean_bias        <- TEResults["Bias",,,]
      res_median_bias      <- TEResults["Bias",,,]
      res_rmse             <- sqrt((TEResults["Bias",,,])^2)
      res_rmse975          <- sqrt((TEResults["Bias",,,])^2)
      res_rmse500          <- sqrt((TEResults["Bias",,,])^2)
      res_rmse025          <- sqrt((TEResults["Bias",,,])^2)
      res_rmse_rel         <- sqrt((TEResults["Bias",,,]/res_median_coef)^2)
      res_mae              <- abs(TEResults["Bias",,,])
      res_mae975           <- abs(TEResults["Bias",,,])
      res_mae500           <- abs(TEResults["Bias",,,])
      res_mae025           <- abs(TEResults["Bias",,,])
      res_mae_rel          <- abs(TEResults["Bias",,,]/res_median_coef)
      res_sd               <- TEResults["Coef.",,,]
      res_mean_se          <- TEResults["SE",,,]
      res_CR_90            <- res_Coverage_90
      res_CR_95            <- res_Coverage_95
      
      res_true_effect      <- InfoResults["true.effect",,]
      res_fraction_treated <- InfoResults["fraction.treated",,]
      res_mean_ps_treated  <- InfoResults["mean.ps.treated",,]
      res_mean_ps_control  <- InfoResults["mean.ps.control",,]
      res_n_dx             <- InfoResults["n.dx",,]
      res_n_yd1x           <- InfoResults["n.yd1x",,]
      res_n_yd0x           <- InfoResults["n.yd0x",,]
      res_n_trim           <- InfoResults["n.trim",,]
      
      res_median_ps_hist_tr <- InfoResults[paste0("hist.", 1:spec$psBins, ".treated"),,]
      res_median_ps_hist_co <- InfoResults[paste0("hist.", 1:spec$psBins, ".control"),,]
      
      if (spec$datatype != "real_nsw"){
        
        res_fs.ps.cor        <- InfoResults["fs.ps.cor",,]
        res_fs.ps.rmse       <- InfoResults["fs.ps.rmse",,]
        res_fs.co1.cor       <- InfoResults["fs.co1.cor",,]
        res_fs.co1.rmse      <- InfoResults["fs.co1.rmse",,]
        res_fs.co0.cor       <- InfoResults["fs.co0.cor",,]
        res_fs.co0.rmse      <- InfoResults["fs.co0.rmse",,]
        
        res_median_ps_hist_tr_true <- InfoResults[paste0("hist.", 1:spec$psBins, ".treated.true"),,]
        res_median_ps_hist_co_true <- InfoResults[paste0("hist.", 1:spec$psBins, ".control.true"),,]
        
      }
      
      # Create final table
      final_table <- array(NA, dim = c(dim(res_mean_coef)[1], dim(res_mean_coef)[2], dim(res_mean_coef)[3], 17))
      final_table <- round(abind(res_mean_coef, res_mean_bias, res_median_bias, 
                                 res_rmse, res_rmse975, res_rmse500, res_rmse025, res_rmse_rel, res_mae, 
                                 res_mae975, res_mae500, res_mae025, res_mae_rel, res_sd, 
                                 res_mean_se, res_CR_95, res_CR_90, along = 4),5)
      
      
    }

    dimnames(final_table)[[1]] <- estimator_list_all
    dimnames(final_table)[[4]] <- c("MeanCoef", "MeanBias", "MedianBias", "RMSE", "RMSE975", "RMSE500", "RMSE025", 
                                    "RMSPE", "MAE", "MAE975", "MAE500", "MAE025", "MAPE", 
                                    "SD", "MeanSE", "CR95", "CR90")

    resultsString <- paste0("data_", spec$datatype, ".RData")
    
    # Check if results .RData file already exists, otherwise create it
    if (file.exists(paste0("Results/", resultsString))){
      
      resultsDF <- get(load(paste0("Results/", resultsString)))

      # check whether results matrix should be made bigger
      if (!is.na(resultsDF[dim(resultsDF)[1]-2,1])){
        matrix_to_add <- matrix(NA, nrow = 10000, ncol = dim(resultsDF)[2])
        resultsDF <- rbind(resultsDF, matrix_to_add)
      }
      
    } else {
      
      resultsDF <- matrix(NA, nrow = 10000, ncol = length(spec_list_all), dimnames = list(c(1:10000), names(spec_list_all)))
      resultsDF <- cbind(resultsDF, NA, NA, NA, NA, NA)
      colnames(resultsDF)[dim(resultsDF)[2]] <- "timestamp"
      colnames(resultsDF)[(dim(resultsDF)[2]-1)] <- "value"
      colnames(resultsDF)[(dim(resultsDF)[2]-2)] <- "performance"
      colnames(resultsDF)[(dim(resultsDF)[2]-3)] <- "variable_set_est_CO"
      colnames(resultsDF)[(dim(resultsDF)[2]-4)] <- "tune_ML_CO"
      colnames(resultsDF)[which( colnames(resultsDF) == "estimator_list_all")] <- "estimator"
      colnames(resultsDF)[which( colnames(resultsDF) == "propScore_methods_list")] <- "propScore"
      colnames(resultsDF)[which( colnames(resultsDF) == "condOutcome_methods_list")] <- "condOutcome"
      colnames(resultsDF)[which( colnames(resultsDF) == "variable_set_est")] <- "variable_set_est_PS"
      colnames(resultsDF)[which( colnames(resultsDF) == "tune_ML")] <- "tune_ML_PS"
      colnames(resultsDF)[which( colnames(resultsDF) == "Dclassification")] <- "Dclassification_PS"
      
    }
    
    
    histString <- paste0("hist_", spec$datatype, ".RData")
    
    # Check if info/hist .RData file already exists, otherwise create it
    if (file.exists(paste0("Results/", histString))){
      
      histDF <- get(load(paste0("Results/", histString)))
      
      # check whether results matrix should be made bigger
      if (!is.na(histDF[dim(histDF)[1]-2,1])){
        matrix_to_add <- matrix(NA, nrow = 10000, ncol = dim(histDF)[2])
        histDF <- rbind(histDF, matrix_to_add)
      }
      
    } else {
      
      histDF <- matrix(NA, nrow = 10000, ncol = length(spec_list_all), dimnames = list(c(1:10000), names(spec_list_all)))
      histDF    <- cbind(histDF, NA)
      colnames(histDF)[dim(histDF)[2]] <- "timestamp"
      histDF <- histDF[, colnames(histDF) != "estimator_list_all"]
      
      histDF_add <- matrix(NA, nrow = 10000, ncol = (16 + 4*spec$psBins))
      histDF <- cbind(histDF, histDF_add)
      
      colnames(histDF)[which( colnames(histDF) == "propScore_methods_list")] <- "propScore"
      colnames(histDF)[which( colnames(histDF) == "condOutcome_methods_list")] <- "condOutcome"
      colnames(histDF)[which( colnames(histDF) == "variable_set_est")] <- "variable_set_est_PS"
      colnames(histDF)[which( colnames(histDF) == "tune_ML")] <- "tune_ML_PS"
      colnames(histDF)[which( colnames(histDF) == "Dclassification")] <- "Dclassification_PS"
      
      indColAfterTime <- which(colnames(histDF)=="timestamp")+1
      
      colnames(histDF)[(indColAfterTime:(indColAfterTime+spec$psBins-1))] <- paste0("hist_co_",1:spec$psBins)
      colnames(histDF)[((indColAfterTime+spec$psBins):(indColAfterTime+2*spec$psBins-1))] <- paste0("hist_tr_",1:spec$psBins)
      colnames(histDF)[((indColAfterTime+2*spec$psBins):(indColAfterTime+3*spec$psBins-1))] <- paste0("hist_co.true_",1:spec$psBins)
      colnames(histDF)[((indColAfterTime+3*spec$psBins):(indColAfterTime+4*spec$psBins-1))] <- paste0("hist_tr.true_",1:spec$psBins)
      
      colnames(histDF)[dim(histDF)[2]-15] <- "fs.co0.rmse"
      colnames(histDF)[dim(histDF)[2]-14] <- "fs.co0.cor"
      colnames(histDF)[dim(histDF)[2]-13] <- "fs.co1.rmse"
      colnames(histDF)[dim(histDF)[2]-12] <- "fs.co1.cor"
      colnames(histDF)[dim(histDF)[2]-11] <- "fs.ps.rmse"
      colnames(histDF)[dim(histDF)[2]-10] <- "fs.ps.cor"
      
      colnames(histDF)[dim(histDF)[2]-9] <- "variable_set_est_CO"
      colnames(histDF)[dim(histDF)[2]-8] <- "tune_ML_CO"
      colnames(histDF)[dim(histDF)[2]-7] <- "n_dx"
      colnames(histDF)[dim(histDF)[2]-6] <- "n_dy1x"
      colnames(histDF)[dim(histDF)[2]-5] <- "n_dy0x"
      colnames(histDF)[dim(histDF)[2]-4] <- "n_trim"
      colnames(histDF)[dim(histDF)[2]-3] <- "true_effect"
      colnames(histDF)[dim(histDF)[2]-2] <- "treated_fraction"
      colnames(histDF)[dim(histDF)[2]-1] <- "mean_ps_control"
      colnames(histDF)[dim(histDF)[2]]   <- "mean_ps_treated"
      
    }
    
    # Save results of final_table 
    index_nonNA <- as.numeric(which(is.na(resultsDF[,1]))[1])
    index_nonNA_hist <- as.numeric(which(is.na(histDF[,1]))[1])
      
    for (d1 in 1:dim(final_table)[1]) { # loop over estimators
      for (d2 in 1:dim(final_table)[2]) { # loop over propScore.grid
        for (d3 in 1:dim(final_table)[3]) { # loop over condOutcome.grid
          for (d4 in 1:dim(final_table)[4]) { # loop over performance measures
          
          resultsDF[index_nonNA, "estimator"] <- estimator_list_all[d1]
          resultsDF[index_nonNA, "propScore"] <- as.character(propScore.grid[d2,"propScore"])
          resultsDF[index_nonNA, "variable_set_est_PS"] <- as.character(propScore.grid[d2,"variable_set_est"])
          resultsDF[index_nonNA, "tune_ML_PS"] <- as.character(propScore.grid[d2,"tune_ML"])
          resultsDF[index_nonNA, "Dclassification_PS"] <- as.character(propScore.grid[d2,"Dclassification"])
          
          resultsDF[index_nonNA, "condOutcome"] <- as.character(condOutcome.grid[d3,"condOutcome"])
          resultsDF[index_nonNA, "variable_set_est_CO"] <- as.character(condOutcome.grid[d3,"variable_set_est"])
          resultsDF[index_nonNA, "tune_ML_CO"] <- as.character(condOutcome.grid[d3,"tune_ML"])
          
          resultsDF[index_nonNA, "performance"] <- dimnames(final_table)[[4]][d4]
          
          if (d1 == 1 && d4 == 1){

            histDF[index_nonNA_hist, "propScore"] <- as.character(propScore.grid[d2,"propScore"])
            histDF[index_nonNA_hist, "variable_set_est_PS"] <- as.character(propScore.grid[d2,"variable_set_est"])
            histDF[index_nonNA_hist, "tune_ML_PS"] <- as.character(propScore.grid[d2,"tune_ML"])
            histDF[index_nonNA_hist, "Dclassification_PS"] <- as.character(propScore.grid[d2,"Dclassification"])
            
            histDF[index_nonNA_hist, "condOutcome"] <- as.character(condOutcome.grid[d3,"condOutcome"])
            histDF[index_nonNA_hist, "variable_set_est_CO"] <- as.character(condOutcome.grid[d3,"variable_set_est"])
            histDF[index_nonNA_hist, "tune_ML_CO"] <- as.character(condOutcome.grid[d3,"tune_ML"])
          }
          
          spec_names <- colnames(spec)
          
          for (sn in spec_names) {
            resultsDF[index_nonNA, sn] <- as.character(spec[1,sn])
            
            if (d1 == 1 && d4 == 1){
              histDF[index_nonNA_hist, sn] <- as.character(spec[1,sn])
            }
          }
          
          resultsDF[index_nonNA, "value"] <- final_table[d1,d2,d3,d4]
          resultsDF[index_nonNA, "timestamp"] <- Sys.time()
          
          if (d1 == 1 && d4 == 1) {
            
            indColAfterTime <- which(colnames(histDF)=="timestamp")+1
            
            histDF[index_nonNA_hist, paste0("hist_co_",1:spec$psBins)] <- as.matrix(res_median_ps_hist_co[,d2,d3])
            histDF[index_nonNA_hist, paste0("hist_tr_",1:spec$psBins)] <- as.matrix(res_median_ps_hist_tr[,d2,d3])
          
            histDF[index_nonNA_hist, "n_dx"] <- res_n_dx[d2,d3]
            histDF[index_nonNA_hist, "n_dy1x"] <- res_n_yd1x[d2,d3]
            histDF[index_nonNA_hist, "n_dy0x"] <- res_n_yd0x[d2,d3]
            histDF[index_nonNA_hist, "n_trim"] <- res_n_trim[d2,d3]
            histDF[index_nonNA_hist, "true_effect"] <- res_true_effect[d2,d3]
            histDF[index_nonNA_hist, "treated_fraction"] <- res_fraction_treated[d2,d3]
            histDF[index_nonNA_hist, "mean_ps_control"] <- res_mean_ps_control[d2,d3]
            histDF[index_nonNA_hist, "mean_ps_treated"]   <- res_mean_ps_treated[d2,d3]
            
            if (spec$datatype != "real_nsw"){
            
              histDF[index_nonNA_hist, "fs.ps.cor"]   <- res_fs.ps.cor[d2,d3]
              histDF[index_nonNA_hist, "fs.ps.rmse"]  <- res_fs.ps.rmse[d2,d3]
              histDF[index_nonNA_hist, "fs.co1.cor"]  <- res_fs.co1.cor[d2,d3]
              histDF[index_nonNA_hist, "fs.co1.rmse"] <- res_fs.co1.rmse[d2,d3]
              histDF[index_nonNA_hist, "fs.co0.cor"]  <- res_fs.co0.cor[d2,d3]
              histDF[index_nonNA_hist, "fs.co0.rmse"] <- res_fs.co0.rmse[d2,d3]
              
              histDF[index_nonNA_hist, paste0("hist_co.true_",1:spec$psBins)] <- as.matrix(res_median_ps_hist_co_true[,d2,d3])
              histDF[index_nonNA_hist, paste0("hist_tr.true_",1:spec$psBins)] <- as.matrix(res_median_ps_hist_tr_true[,d2,d3])
              
            }
            
            histDF[index_nonNA_hist, "timestamp"] <- Sys.time()
            index_nonNA_hist <- index_nonNA_hist + 1
          }
          
          index_nonNA <- index_nonNA + 1
          }
        }
      }
    }
    
    # Keep only unique rows
    resultsDF <- unique(resultsDF)
    histDF <- unique(histDF)
    
    # Keep only the latest estimated specification
    resultsDF2 <- resultsDF[order(resultsDF[,"timestamp"], decreasing = TRUE),]
    resultsDF3 <- resultsDF2[, !colnames(resultsDF2) %in% c("timestamp", "value")]
    resultsDF <- resultsDF2[!duplicated(resultsDF3),]
    
    indx <- startsWith(colnames(histDF), "hist") | startsWith(colnames(histDF), "treated") | 
      startsWith(colnames(histDF), "mean") | startsWith(colnames(histDF), "timestamp") | 
      startsWith(colnames(histDF), "n_d")   | startsWith(colnames(histDF), "true") |
      startsWith(colnames(histDF), "n_tr") | startsWith(colnames(histDF), "fs.")  
    
    histDF2 <- histDF[order(histDF[,"timestamp"], decreasing = TRUE),]
    histDF3 <- histDF2[, !indx ]
    histDF  <- histDF2[!duplicated(histDF3),]
    
    save_paths_data <- c(paste0("Results/", resultsString), paste0("MLE_Shiny2/Results/", resultsString))
    save_paths_hist <- c(paste0("Results/", histString), paste0("MLE_Shiny2/Results/", histString))
    
    sapply(save_paths_data, function(x) save(resultsDF, file = x))
    sapply(save_paths_hist, function(x) save(histDF, file = x))
  
    save(results_reps, file = paste0("Results/Raw/results_reps_", format(Sys.time(), "%y_%m_%d_%H_%M"), ".RData"))
    
}
