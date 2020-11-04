#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file defines the "Estimation Settings" tab of the UI
#
#########################################################################################################

list(
  
  # Select Estimators
  selectInput(inputId  = "estimators",
              label    = "Estimators:",
              choices  = c("Simple OLS"                           = "SimpleOLS",
                           "Regression Estimator"                 = "Regression",
                           "Inverse Probability Weighting"        = "IPW",
                           "Matching on PS"                       = "1NNMwR",
                           "BC Matching on PS"                    = "BC1NNMwR",
                           "Doubly Robust"                        = "DoubleML"),
              selected = c("SimpleOLS", "Regression", "IPW", "1NNMwR", "BC1NNMwR", "DoubleML"), multiple = TRUE),
  
  # Select Misspecification
  conditionalPanel(condition = "input.comparison_mode != 'analysis' & input.datatype == 'simulate_diamond1'", 
                   selectInput(inputId  = "misspecification",
                               label    = "PS and Cond. Outcome Misspecification:",
                               choices  = c("MS I: Both PS and Cond. Outcome correctly specified"        = "bothCorrect",
                                            "MS II: PS correctly specified, Cond. Outcome misspecified"  = "PSCorrect",
                                            "MS III: Cond. Outcome correctly specified, PS misspecified" = "COCorrect",
                                            "MS IV: Both PS and Cond. Outcome misspecified"              = "bothMisspecified"),
                               selected = "bothMisspecified")),
  
  conditionalPanel(condition = "(input.misspecification != 'bothCorrect' )", 
                   selectInput(inputId  = "tuneML",
                               label    = "Tune parameters with Caret:",
                               choices  = c("Yes" = "1",
                                            "No"  = "0"),
                               selected = "0")
                   
  ),
  
  conditionalPanel(condition = "(input.misspecification != 'COCorrect' | input.misspecification != 'bothMisspecified')", 
                   selectInput(inputId  = "Dclassification_PS",
                               label    = "Estimate D~X as classification:",
                               choices  = choices_Dclassification_PS,
                               selected = "0")
                   
  ),
  
  conditionalPanel(condition = "input.comparison_mode != 'analysis' | (input.comparison_mode == 'analysis' & 
                 input.analysisDim != 'crossFitFolds')", 
                   
                   # Select Cross-Fit Folds
                   selectInput(inputId  = "crossFitFolds", 
                               label    = "Cross-Fitting:",
                               choices  = c("No Cross-Fitting"  = "1", 
                                            "2 Folds"  = "2",
                                            "5 Folds"  = "5",
                                            "10 Folds"  = "10"), 
                               selected = "5")),
  
  conditionalPanel(condition = "(input.comparison_mode != 'analysis' & input.crossFitFolds>1) | (input.comparison_mode == 'analysis' & 
                 input.analysisDim != 'crossFitFolds' & input.crossFitFolds>1)", 
                   
                   selectInput(inputId  = "sampleSplits", 
                               label    = "Repeated Sample-Splitting:",
                               choices  = c("1 Split"  = "1", 
                                            "10 Splits"  = "10",
                                            "50 Splits" = "50"), 
                               selected = "1"))
  
)