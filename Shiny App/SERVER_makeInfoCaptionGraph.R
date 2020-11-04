#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file adds graph captions 
#
#########################################################################################################

if (input$datatype == "simulate_busso" | input$datatype == "simulate_diamond1"){
  
  if (input$comparison_mode == "within") {
    
    html_text <- paste0("Note: Simulation based on 5000 replications. The bars display the relative change in ", input$performance_graph, " of machine learning based estimation of the ",
                         ifelse(input$datatype == "simulate_diamond1", "ATE", "ATT"), " (random forest in the left panel, elastic net in the right panel), compared to conventional based estimation of the ",
                         ifelse(input$datatype == "simulate_diamond1", "ATE", "ATT"), " (OLS/Logit). A negative value indicates that the ", input$performance_graph, " of the machine learning based estimator was lower, i.e. that the treatment effect was estimated more accurately with machine learning methods.")
    
  } else if (input$comparison_mode == "between") {
    
    if (input$misspecification == "bothCorrect") {
      
      html_text <- paste0("Note: The bars indicate the ", input$performance_graph, " of the ",
                          ifelse(input$datatype == "simulate_diamond1", "ATE", "ATT"), " estimator over 5000 simulation replications. Red bars represent estimators which use conventional methods to estimate the conditional outcome means (OLS) and the propensity score (Logit). The blue bar represents the OLS estimator regressing the outcome on the treatment indicator and the ten covariates.")
      
    } else if (input$misspecification == "PSCorrect") {
      
      html_text <- paste0("Note: The bars indicate the ", input$performance_graph, " of the ",
                          ifelse(input$datatype == "simulate_diamond1", "ATE", "ATT"), " estimator over 5000 simulation replications. Green bars represent estimators which use machine learning methods (random forests and elastic net) to estimate the conditional outcome means. Red bars represent estimators which use conventional methods (OLS) to estimate the conditional outcome means. The blue bar represents the simple OLS estimator. The results for random forest (RF) are shown in the left panel, the results for elastic net are shown in the right panel.")
      
    } else if (input$misspecification == "COCorrect") {
      
      html_text <- paste0("Note: The bars indicate the ", input$performance_graph, " of the ",
                          ifelse(input$datatype == "simulate_diamond1", "ATE", "ATT"), " estimator over 5000 simulation replications. Green bars represent estimators which use machine learning methods (random forests and elastic net) to estimate the propensity score. Red bars represent estimators which use conventional methods (Logit) to estimate the propensity score. The blue bar represents the simple OLS estimator. The results for random forest (RF) are shown in the left panel, the results for elastic net are shown in the right panel.")
      
    } else {
      
      html_text <- paste0("Note: The bars indicate the ", input$performance_graph, " of the ",
                          ifelse(input$datatype == "simulate_diamond1", "ATE", "ATT"), " estimator over 5000 simulation replications. Green bars represent estimators which use machine learning methods (random forests and elastic net) to estimate the conditional outcome means and the propensity score. Red bars represent estimators which use conventional methods (OLS and Logit) to estimate the conditional outcome means and the propensity score. The blue bar represents the simple OLS estimator. The results for random forest (RF) are shown in the left panel, the results for elastic net are shown in the right panel.")
      
    }  
    
  } else {
    
    if (input$analysisDim == "linAdd") {
      
      text1 <- "Within a subgraph, the x-axis plots three degree of linearity and additivity. 'L' corresponds to a DGP where both the outcome specification and the treatment specification are linear and additive, i.e. include only the main effects. 'MNL' and 'NL' correspond to DGPs where both the outcome specification and the treatment specification are nonlinear and nonadditive. 'MNL' includes the main effects, one quadratic term and four interaction terms. 'NL' includes the main effects, three quadratic terms, and ten interaction terms."
      
    } else if (input$analysisDim == "select_into_D") {
      
      text1 <- "Within a subgraph, the x-axis plots phi, the strength of selection into treatment. The larger phi, the stronger the selection into treatment. With phi = 0, selection into treatment is random. The default specification corresponds to phi = 1."
            
    } else if (input$analysisDim == "trimPS") {
      
      text1 <- "Within a subgraph, the x-axis plots the fraction of trimmed propensity scores."
      
    } else if (input$analysisDim == "n_sim") {
      
      text1 <- "Within a subgraph, the x-axis plots the number of observations."
      
    } else if (input$analysisDim == "Ynoise") {
      
      text1 <- "Within a subgraph, the x-axis plots whether the outcome specification includes an error term. The parameter represents a multiplicative factor on a normally distributed error term, i.e. the larger the multiplicative factor, the larger the variance of the error term."
      
    } else if (input$analysisDim == "crossFitFolds") {
      
      text1 <- "Within a subgraph, the x-axis plots the number of cross-fit folds. If the number of cross-fit folds is equal to 1, no cross-fitting was applied."
      
    } else if (input$analysisDim == "sampleSplits") {
      
      text1 <- "Within a subgraph, the x-axis plots the number of repeated sample-splits. If the number of repeated sample-splits is equal to 1, no repeated sample-splitting was applied."
      
    } else if (input$analysisDim == "tuneML") {
      
      text1 <- "Within a subgraph, the x-axis plots whether tuning was used (Yes) or not (No)"
      
    } else if (input$analysisDim == "Dclassification_PS") {
      
      text1 <- "Within a subgraph, the x-axis plots whether the propensity score was estimated as a classification problem."
      
    }
    
    html_text <- paste0("Note: In each panel, the columns represent the treatment effect estimators, the rows indicate whether random forest (RF) or elastic net (Elnet) was used to estimate the propensity score and/or the conditional outcome means. ",
                        text1, " To improve readability, the points are slightly offset horizontally. The shape of the points represents the simple OLS estimator (square), the conventional based estimators (circle) and the machine learning based estimators (triangle).")
    
  }
    
    html_text <- HTML(html_text)
    
  
} else { # real_nsw
  
  if (input$subsample == "Lalonde"){
    subsample.string <- ""
  } else if (input$subsample == "Dehejia"){
    subsample.string <- "(Dehejia/Wahba subsample) "
  } else if (input$subsample == "SmithTodd"){
    subsample.string <- "(Smith/Todd subsample) "
  }
  
  if (input$gender == "men"){
    comp.group.string <- toupper(input$comp_group_men)
  } else if (input$gender == "women"){
    comp.group.string <- toupper(input$comp_group_women)
  }
  
  if (input$comparison_mode == "within") {
    
    html_text <- paste0("Note: Results for original Lalonde data ", subsample.string, 
                             input$gender, " sample. The dataset consists of the ",
                             ifelse(input$exper_group == "control", "control", "treated"), " individuals from the experimental group and the comparison group from the ",
                             comp.group.string, " dataset. The bars display the relative change in absolute bias of machine learning based estimation of the ATT (random forest in the left panel, elastic net in the right panel), compared to conventional based estimation of the ATT (OLS/Logit). A negative value indicates that the absolute bias of the machine learning based estimator was lower, i.e. that the treatment effect was estimated more accurately with machine learning methods.")
    
  } else if (input$comparison_mode == "between") {
    
    html_text <- paste0("Note: Results for original Lalonde data ", subsample.string, 
                        input$gender, " sample. The dataset consists of the ",
                        ifelse(input$exper_group == "control", "control", "treated"), " individuals from the experimental group and the comparison group from the ",
                        comp.group.string, " dataset. The bars indicate the absolute bias of the ATT estimator. Green bars represent estimators which use machine learning methods (random forests and elastic net) to estimate the conditional outcome means and the propensity score. Red bars represent estimators which use conventional methods (OLS and Logit) to estimate the conditional outcome means and the propensity score. The blue bar represents the simple OLS estimator. The results for random forest (RF) are shown in the left panel, the results for elastic net are shown in the right panel.")
    
  } else {
    
    if (input$analysisDim == "linAdd") {
      
      text1 <- "Within a subgraph, the x-axis plots three degree of linearity and additivity. 'L' corresponds to a DGP where both the outcome specification and the treatment specification are linear and additive, i.e. include only the main effects. 'MNL' and 'NL' correspond to DGPs where both the outcome specification and the treatment specification are nonlinear and nonadditive. 'MNL' includes the main effects, one quadratic term and four interaction terms. 'NL' includes the main effects, three quadratic terms, and ten interaction terms."
      
    } else if (input$analysisDim == "select_into_D") {
      
      text1 <- "Within a subgraph, the x-axis plots phi, the strength of selection into treatment. The larger phi, the stronger the selection into treatment. With phi = 0, selection into treatment is random. The default specification corresponds to phi = 1."
      
    } else if (input$analysisDim == "trimPS") {
      
      text1 <- "Within a subgraph, the x-axis plots the fraction of trimmed propensity scores."
      
    } else if (input$analysisDim == "n_sim") {
      
      text1 <- "Within a subgraph, the x-axis plots the number of observations."
      
    } else if (input$analysisDim == "Ynoise") {
      
      text1 <- "Within a subgraph, the x-axis plots whether the outcome specification includes an error term. The parameter represents a multiplicative factor on a normally distributed error term, i.e. the larger the multiplicative factor, the larger the variance of the error term."
      
    } else if (input$analysisDim == "crossFitFolds") {
      
      text1 <- "Within a subgraph, the x-axis plots the number of cross-fit folds. If the number of cross-fit folds is equal to 1, no cross-fitting was applied."
      
    } else if (input$analysisDim == "sampleSplits") {
      
      text1 <- "Within a subgraph, the x-axis plots the number of repeated sample-splits. If the number of repeated sample-splits is equal to 1, no repeated sample-splitting was applied."
      
    } else if (input$analysisDim == "tuneML") {
      
      text1 <- "Within a subgraph, the x-axis plots whether tuning was used (Yes) or not (No)"
      
    } else if (input$analysisDim == "Dclassification_PS") {
      
      text1 <- "Within a subgraph, the x-axis plots whether the propensity score was estimated as a classification problem."
      
    }
    
    
    html_text <- paste0("Note: In each panel, the columns represent the treatment effect estimators, the rows indicate whether random forest (RF) or elastic net (Elnet) was used to estimate the propensity score and/or the conditional outcome means. ",
                        text1, " To improve readability, the points are slightly offset horizontally. The shape of the points represents the simple OLS estimator (square), the conventional based estimators (circle) and the machine learning based estimators (triangle).")
    
    
  }
  
  html_text <- HTML(html_text)
 
}