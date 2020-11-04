#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file creates the table for the user selected comparison mode
#
#########################################################################################################

perf_graph <- input$performance_graph

if (input$comparison_mode == "within") {
  
  data_within <- get_data_within()
  mle_table <- data_within$data_within_table
  
  colnames(mle_table) <- c("Estimator", "ML Method", "Performance", "Conv.", "ML", "ML rel. change.")
  mle_table
  
} else if (input$comparison_mode == "between") {
  
  data_between <- get_data_between()
  mle_table <- data_between$data_between_table
  
  colnames(mle_table) <- c("Estimator", "Conv./ML", "Bias", "SD", "RMSE")
  mle_table
  
} else if (input$comparison_mode == "analysis") {
  
  data_analysis <- get_data_analysis() 
  
  mle_table <- data_analysis$data_analysis_table
  
  colnames(mle_table)[1] <- c("Misspecification")
  colnames(mle_table)[2] <- c("Estimator")
  colnames(mle_table)[3] <- c("Conv./ML")
  
  mle_table
  
}

mle_table <- unique(mle_table)

mle_table