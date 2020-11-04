#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file creates the first stage table displaying the correlations/RMSE of estimated and
#              true propensity scores/conditional outcome means
#
#########################################################################################################

data_subset_list <- get_data_subset()

if (input$datatype == "simulate_diamond1"){
  data_fs_oracle <- dplyr::filter(data_subset_list$info, 
                                  (variable_set_est_PS == "Oracle" & variable_set_est_CO == "Oracle")) %>%
    dplyr::select(fs.ps.cor, fs.co1.cor, fs.co0.cor, fs.ps.rmse, fs.co1.rmse, fs.co0.rmse) %>%
    mutate_all(function(f) as.numeric(levels(f))[f])
  
  nb.methods <- 4
  
} else {
  
  nb.methods <- 3
  
}

data_fs_main <- dplyr::filter(data_subset_list$info, 
                              (variable_set_est_PS == "Main" & variable_set_est_CO == "Main")) %>%
  dplyr::select(fs.ps.cor, fs.co1.cor, fs.co0.cor, fs.ps.rmse, fs.co1.rmse, fs.co0.rmse) %>%
  mutate_all(function(f) as.numeric(levels(f))[f])

data_fs_rf <- dplyr::filter(data_subset_list$info, 
                            (condOutcome == "RF" & propScore == "RF")) %>%
  dplyr::select(fs.ps.cor, fs.co1.cor, fs.co0.cor, fs.ps.rmse, fs.co1.rmse, fs.co0.rmse) %>%
  mutate_all(function(f) as.numeric(levels(f))[f])

data_fs_lasso <- dplyr::filter(data_subset_list$info, 
                               (condOutcome == "Lasso" & propScore == "Lasso")) %>%
  dplyr::select(fs.ps.cor, fs.co1.cor, fs.co0.cor, fs.ps.rmse, fs.co1.rmse, fs.co0.rmse) %>%
  mutate_all(function(f) as.numeric(levels(f))[f])

res <- matrix(NA, nrow = nb.methods, ncol = 6)

res[1,] <- as.matrix(data_fs_main)
res[2,] <- as.matrix(data_fs_rf)
res[3,] <- as.matrix(data_fs_lasso)

if (input$datatype == "simulate_diamond1"){
  
 res[4,] <- as.matrix(data_fs_oracle)
 rownames(res) <- c("Logit/OLS Main", "Random Forest", "Elastic Net", "Logit/OLS Oracle")
 
} else {
  
  rownames(res) <- c("Logit/OLS Main", "Random Forest", "Elastic Net")
  
}

colnames(res) <- c("Cor PS", "Cor Y1", "Cor Y0", "RMSE PS", "RMSE Y1", "RMSE Y0")

na.ind <- colSums(is.na(res))==0
res2   <- res[,na.ind]

mle_fs_table0 <- as.data.frame(round(res2, 3))

mle_fs_table <- cbind(rownames(res), mle_fs_table0)
colnames(mle_fs_table)[1] <- "Method"

mle_fs_table
