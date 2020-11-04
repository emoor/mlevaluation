#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file defines the "General Settings" tab of the UI
#
#########################################################################################################

list(
    
    # Select Data type
    selectInput(inputId  = "datatype",
                label    = "Data type:",
                choices  = c("Stylized Simulation Study"                 = "simulate_diamond1",
                             "Empirical Simulation Study: LaLonde data"  = "simulate_busso",
                             "Within-Study Comparison: LaLonde data"     = "real_nsw"),
                selected = "simulate_diamond1"),
    
    # Select comparison mode
    selectInput(inputId  = "comparison_mode",
                label    = "Comparison mode:",
                choices  = c("Within-Estimator"    = "within",
                             "Between-Estimator"   = "between",
                             "Analysis"            = "analysis"),
                selected = "between"),
    
    conditionalPanel(condition = "input.datatype == 'real_nsw'",
                     
                     selectInput(inputId  = "draw_bs", 
                                 label    = "Original/Bootstrap:",
                                 choices  = c("Original dataset" = "0",
                                              "Bootstrap samples" = "1"),
                                 selected = "1")),
    
    conditionalPanel(condition = "input.comparison_mode == 'analysis'", #####################
                     
                     selectInput(inputId = "analysisDim",
                                 label   = "Analysis Dimension:",
                                 choices = c("Linearity and Additivity" = "linAdd",
                                             "Strength Selection into Treatment" = "select_into_D",
                                             "Noise in Outcome Specification" = "Ynoise",
                                             "Number of Observations" = "n_sim",
                                             "Cross-Fitting" = "crossFitFolds",
                                             "Repeated Sample-Splitting" = "sampleSplits",
                                             "Parameter Tuning" = "tuneML",
                                             "D~X as Classification" = "Dclassification_PS",
                                             "Trim Propensity Scores" = "trimPS"),
                                 selected = "linAdd"),
                     
                     selectInput(inputId = "analysisMisspec",
                                 label   = "Show Analysis for:",
                                 choices = misspec_choices,
                                 selected = c("bothMisspecified")),
                     
                     conditionalPanel(condition = "input.analysisDim == 'linAdd'", ################
                                      
                                      selectInput(inputId  = "linAddParam", 
                                                  label    = "Linearity, Additivity, or both:",
                                                  choices  = c("Linearity (Interactions)" = "lin",
                                                               "Additivity (Squared terms)" = "add",
                                                               "Linearity and Additivity" = "linAdd"),
                                                  selected = "linAdd"), 
                                      
                                      selectInput(inputId  = "linAddrelation", 
                                                  label    = "D~X, Y~X, or both:",
                                                  choices  = c("D~X" = "DX",
                                                               "Y~X" = "YX",
                                                               "D~X and Y~X" = "DXYX"),
                                                  selected = "DXYX"),
                                      
                                      conditionalPanel(condition = "input.linAddrelation != 'DXYX'",
                                                       selectInput(inputId  = "linAddFixed", 
                                                                   label    = "Relation of not analyzed param:",
                                                                   choices  = c("Linear and Additive" = "A",
                                                                                "Moderate non-linear and non-additive" = "G"),
                                                                   selected = "A"))
                                      

                     )
                     
    )
    
)