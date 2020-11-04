#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file creates the graph for the user selected comparison mode
#
#########################################################################################################

if (input$comparison_mode == "within") {
  
  data_within <- get_data_within()
  data_within_graph <- data_within$data_within_graph
  
  analysis_y_lab <- paste0("Relative ", ifelse(input$performance_graph == "MeanBias", "|Bias|", input$performance_graph), " change")
  
  mle_graph <- ggplot(data_within_graph, aes(x = estimator, y = relChange)) + 
    geom_bar(stat = "identity" , width = 0.7, fill = "grey75") +
    facet_grid(. ~ method) +
    labs(y = analysis_y_lab, 
         x = paste0(ifelse(input$datatype == "simulate_diamond1", "ATE", "ATT"), " Estimator")) +
    coord_flip()
  
} else if (input$comparison_mode == "between") {
  
  data_between <- get_data_between()
  data_between_graph <- data_between$data_between_graph
  
  analysis_y_lab <- ifelse(input$performance_graph == "MeanBias", "|Bias|", input$performance_graph)
    
  mle_graph <- ggplot(data_between_graph, aes(x = combName, y = abs(value), fill = MLConv)) + 
      geom_bar(width = 0.7, stat = "identity") +
      labs(y = analysis_y_lab, x = paste0(ifelse(input$datatype == "simulate_diamond1", "ATE", "ATT"), " Estimator"), fill = "T") +
      coord_flip()
    
  # Facet grid only if more than one method available
  if (length(unique(data_between_graph$method)) > 1){
    mle_graph <- mle_graph + facet_grid(. ~ method)
  }
  
} else if (input$comparison_mode == "analysis") {
  
  data_analysis <- get_data_analysis()
  data_analysis_graph <- data_analysis$data_analysis_graph
  
  data_analysis_graph <- data_analysis_graph %>%
    mutate("ID" = paste(estimator, MLConv))
  
  x_label1 <- toString(data_analysis_graph$name[which(data_analysis_graph$indicator==1)[1]])
  x_label2 <- toString(data_analysis_graph$name[which(data_analysis_graph$indicator==2)[1]])
  x_label3 <- toString(data_analysis_graph$name[which(data_analysis_graph$indicator==3)[1]])
  x_label4 <- toString(data_analysis_graph$name[which(data_analysis_graph$indicator==4)[1]])
  x_label5 <- toString(data_analysis_graph$name[which(data_analysis_graph$indicator==5)[1]])
  x_label6 <- toString(data_analysis_graph$name[which(data_analysis_graph$indicator==6)[1]])
  
  if (input$analysisDim == "linAdd"){
    analysis_x_lab <- "Linearity/Additivity"
    x_label1 <- "L"
    x_label2 <- "MNL"
    x_label3 <- "NL"
  } else if (input$analysisDim == "n_sim"){
    analysis_x_lab <- "Number of Observations"
  } else if (input$analysisDim == "crossFitFolds"){
    analysis_x_lab <- "Number of Cross-Fitting Folds"
  } else if (input$analysisDim == "sampleSplits"){
    analysis_x_lab <- "Repeated Sample Splitting"
  } else if (input$analysisDim == "select_into_D"){
    analysis_x_lab <- expression(paste("Strength Selection into Treatment (", phi, ")"))
  } else if (input$analysisDim == "D_balanced"){
    analysis_x_lab <- "Balanced Treatment/Control Sample PSID"
  } else if (input$analysisDim == "trimPS"){
    analysis_x_lab <- "Propensity Score Trimming (fraction trimmed)"
  } else if (input$analysisDim == "misspecification"){
    analysis_x_lab <- "Misspecification"
  } else if (input$analysisDim == "tuneML"){
    analysis_x_lab <- "Hyperparameter Tuning"
  } else if (input$analysisDim == "Dclassification_PS"){
    analysis_x_lab <- "Estimate D~X as classification problem"
  } else if (input$analysisDim == "sampleInteractions"){
    analysis_x_lab <- "Sample DGP interaction terms: No (0) / Yes (1)"
  } else if (input$analysisDim == "Ynoise"){
    analysis_x_lab <- "Random noise in outcome specification (multiplicative factor)"
  }
  
  analysis_y_lab <- ifelse(input$performance_graph == "MeanBias", "|Bias|", input$performance_graph)

  mle_graph <- ggplot(data_analysis_graph, aes(x = factor(indicator), y = abs(value), group = ID, shape = MLConv)) +
    facet_grid(method ~ estimator) + labs(x = analysis_x_lab, y = analysis_y_lab) +
    scale_x_discrete(labels=c("1" = x_label1, "2" = x_label2, "3" = x_label3, "4" = x_label4, "5" = x_label5, "6" = x_label6))
  
} 

mle_graph