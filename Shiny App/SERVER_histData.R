#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file extracts the data required to create the histogram of propensity scores
#
#########################################################################################################

data_subset_list <- get_data_subset()

hist_treat_conv <- dplyr::select(data_subset_list$info.conv, starts_with("hist_tr_")) %>%
  gather(key = "bin" , value = "value") %>% 
  mutate(group = "Treated", mlConv = "Logit")

hist_cont_conv <-  dplyr::select(data_subset_list$info.conv, starts_with("hist_co_")) %>%
  gather(key = "bin" , value = "value") %>% 
  mutate(group = "Control", mlConv = "Logit")

hist_treat_ml <-   dplyr::select(data_subset_list$info.ml, starts_with("hist_tr_")) %>%
  gather(key = "bin" , value = "value") %>% 
  mutate(group = "Treated", mlConv = "ML")

hist_cont_ml <-    dplyr::select(data_subset_list$info.ml, starts_with("hist_co_")) %>%
  gather(key = "bin" , value = "value") %>% 
  mutate(group = "Control", mlConv = "ML")

hist_treat_true <-   dplyr::select(data_subset_list$info.conv, starts_with("hist_tr.true_")) %>%
  gather(key = "bin" , value = "value") %>% 
  mutate(group = "Treated", mlConv = "True")

hist_cont_true <-    dplyr::select(data_subset_list$info.conv, starts_with("hist_co.true_")) %>%
  gather(key = "bin" , value = "value") %>% 
  mutate(group = "Control", mlConv = "True")

if (input$misspecification == "PSCorrect"){
  
  hist_rf <- data_subset_list$info.ml %>% dplyr::filter(condOutcome == "RF")
  hist_lasso <- data_subset_list$info.ml %>% dplyr::filter(condOutcome == "Lasso")
  
} else {
  
  hist_rf <- data_subset_list$info.ml %>% dplyr::filter(propScore == "RF")
  hist_lasso <- data_subset_list$info.ml %>% dplyr::filter(propScore == "Lasso")
  
} 

hist_treat_rf <-   dplyr::select(hist_rf, starts_with("hist_tr_")) %>%
  gather(key = "bin" , value = "value") %>% 
  mutate(group = "Treated", mlConv = "RF")

hist_cont_rf <-    dplyr::select(hist_rf, starts_with("hist_co_")) %>%
  gather(key = "bin" , value = "value") %>% 
  mutate(group = "Control", mlConv = "RF")

hist_treat_lasso <-   dplyr::select(hist_lasso, starts_with("hist_tr_")) %>%
  gather(key = "bin" , value = "value") %>% 
  mutate(group = "Treated", mlConv = "Lasso")

hist_cont_lasso <-    dplyr::select(hist_lasso, starts_with("hist_co_")) %>%
  gather(key = "bin" , value = "value") %>% 
  mutate(group = "Control", mlConv = "Lasso")


hist_data <- rbind(hist_treat_conv, hist_cont_conv, hist_treat_rf, hist_cont_rf, 
                   hist_treat_lasso, hist_cont_lasso, hist_treat_true, hist_cont_true)

hist_data$bin <- gsub('hist_co.*_', '', hist_data$bin)
hist_data$bin <- gsub('hist_tr.*_', '', hist_data$bin)
hist_data$bin <- as.numeric(hist_data$bin)

nb_bins <- length(unique(hist_data$bin))

hist_data$bin <- hist_data$bin/nb_bins  -  1/(2*nb_bins)

hist_data$mlConv = factor(hist_data$mlConv, levels = c("True", "Logit", "RF", "Lasso"), 
                          labels = c("True", "Logit", "RF", "Elnet"))

hist_data