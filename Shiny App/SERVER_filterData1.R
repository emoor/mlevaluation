#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file filters data and info according to selected user input or specified values
#
#########################################################################################################

data <- get_data()

info <- get_info()


if (input$datatype == "real_nsw") {
    
    data <- dplyr::filter(data, reps == 5000 & draw_bs == input$draw_bs)
    info <- dplyr::filter(info, reps == 5000 & draw_bs == input$draw_bs)
  
} else if (input$datatype == "simulate_busso" | input$datatype == "simulate_diamond1") {
  
  data <- dplyr::filter(data, reps == 5000)
  info <- dplyr::filter(info, reps == 5000)
  
}

if (input$datatype == "simulate_diamond1") {
  
  data <- dplyr::filter(data, param_of_interest == "ATE")
  info <- dplyr::filter(info, param_of_interest == "ATE")
  
} else if (input$datatype == "simulate_busso" | input$datatype == "real_nsw") {
  
  data <- dplyr::filter(data, param_of_interest == "ATT")
  info <- dplyr::filter(info, param_of_interest == "ATT")
  
}

if (input$comparison_mode != "analysis" | (input$comparison_mode == "analysis" & input$analysisDim != "tuneML")) {
  
  data <- dplyr::filter(data, (is.na(tune_ML_CO) | tune_ML_CO == input$tuneML))
  info <- dplyr::filter(info, (is.na(tune_ML_CO) | tune_ML_CO == input$tuneML))
  
  data <- dplyr::filter(data, (is.na(tune_ML_PS) | tune_ML_PS == input$tuneML))
  info <- dplyr::filter(info, (is.na(tune_ML_PS) | tune_ML_PS == input$tuneML))
  
}

if (input$comparison_mode != "analysis" | (input$comparison_mode == "analysis" & input$analysisDim != "trimPS")) {
  
  data <- dplyr::filter(data, trimPS == input$trimPS )
  info <- dplyr::filter(info, trimPS == input$trimPS )
  
}

if (input$comparison_mode != "analysis" | (input$comparison_mode == "analysis" & input$analysisDim != "Dclassification_PS")) {
  
  data <- dplyr::filter(data, (is.na(Dclassification_PS ) | Dclassification_PS  == input$Dclassification_PS))
  info <- dplyr::filter(info, (is.na(Dclassification_PS ) | Dclassification_PS  == input$Dclassification_PS))
  
}

if (input$comparison_mode != "analysis" | (input$comparison_mode == "analysis" & input$analysisDim != "crossFitFolds")) {
  
  data <- dplyr::filter(data, crossFitFolds == input$crossFitFolds )
  info <- dplyr::filter(info, crossFitFolds == input$crossFitFolds )
  
}

if (input$comparison_mode != "analysis" | (input$comparison_mode == "analysis" & input$analysisDim != "sampleSplits")) {
  
  data <- dplyr::filter(data, sampleSplits == sampleSplits_reac() )
  info <- dplyr::filter(info, sampleSplits == sampleSplits_reac() )
  
}

###################################################################################################################

if (input$datatype == "simulate_diamond1") {
  
  if (input$comparison_mode != "analysis" | (input$comparison_mode == "analysis" & input$analysisDim != "linAdd")) {
    
    data <- dplyr::filter(data, scenarioD == input$scenarioD & 
                            scenarioY == input$scenarioY)
    info <- dplyr::filter(info, scenarioD == input$scenarioD & 
                            scenarioY == input$scenarioY)
  }
}

if (input$datatype == "simulate_diamond1" | input$datatype == "simulate_busso") {
  
  if (input$comparison_mode != "analysis" | (input$comparison_mode == "analysis" & input$analysisDim != "n_sim")) {
    
    data <- dplyr::filter(data, n_sim == input$n_sim)
    info <- dplyr::filter(info, n_sim == input$n_sim)
    
  }
  
  if (input$comparison_mode != "analysis" | (input$comparison_mode == "analysis" & input$analysisDim != "select_into_D")) {
    
    data <- dplyr::filter(data, select_into_D == input$select_into_D)
    info <- dplyr::filter(info, select_into_D == input$select_into_D)
    
  }
  
  if (input$comparison_mode != "analysis" | (input$comparison_mode == "analysis" & input$analysisDim != "Ynoise")) {
    
    data <- dplyr::filter(data, Ynoise == input$Ynoise )
    info <- dplyr::filter(info, Ynoise == input$Ynoise )
    
  }
  
  data <- dplyr::filter(data, treat_frac_cons == 0 )
  info <- dplyr::filter(info, treat_frac_cons == 0 )
  

} # Filter diamond1 and busso end


###################################################################################################################
  
if (input$datatype == "real_nsw"){
  
  if (input$gender == "men") {
    
    data <- dplyr::filter(data, nsw_gender == "men" & 
                            comp_group == input$comp_group_men &
                            exper_group == input$exper_group &
                            nsw_sample == input$subsample)
    
    info <- dplyr::filter(info, nsw_gender == "men" & 
                            comp_group == input$comp_group_men &
                            exper_group == input$exper_group &
                            nsw_sample == input$subsample)
    
  } else if (input$gender == "women") {
    
    data <- dplyr::filter(data, nsw_gender == "women" & 
                            comp_group == input$comp_group_women &
                            exper_group == input$exper_group &
                            nsw_sample == input$subsample)
    
    info <- dplyr::filter(info, nsw_gender == "women" & 
                            comp_group == input$comp_group_women &
                            exper_group == input$exper_group &
                            nsw_sample == input$subsample)
  }

}

# Select only user selected estimators
data <- data %>% 
  dplyr::filter(estimator %in% input$estimators)

data_subset_list1 <- list(data = data, info = info)

data_subset_list1
