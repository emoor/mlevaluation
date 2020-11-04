#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file further filters data and info according to selected user input or specified values
#
#########################################################################################################

data <- data_subset_list1$data
info <- data_subset_list1$info

# Select subset depending on misspecification
if (input$datatype == "simulate_diamond1"){
  
  if (input$misspecification == "bothCorrect"){
    
    data.conv <- dplyr::filter(data, propScore == "Logit" & 
                                 variable_set_est_PS == "Oracle" &
                                 condOutcome == "OLS" &
                                 variable_set_est_CO == "Oracle")
    
    #data.ml will be deleted, i.e. not shown in graph (because ML not possible with misspec == "bothCorrect")
    data.ml <- dplyr::filter(data, propScore == "Logit" & 
                               variable_set_est_PS == "Oracle" &
                               condOutcome == "OLS" &
                               variable_set_est_CO == "Oracle")
    
    info.conv <- dplyr::filter(info, propScore == "Logit" & 
                                 variable_set_est_PS == "Oracle" &
                                 condOutcome == "OLS" &
                                 variable_set_est_CO == "Oracle")
    
    #info.ml will be deleted, i.e. not shown in graph (because ML not possible with misspec == "bothCorrect")
    info.ml <- dplyr::filter(info, propScore == "Logit" & 
                               variable_set_est_PS == "Oracle" &
                               condOutcome == "OLS" &
                               variable_set_est_CO == "Oracle")
    
  } else if (input$misspecification == "PSCorrect"){
    
    data.conv <- dplyr::filter(data, propScore == "Logit" & 
                                 variable_set_est_PS == "Oracle" &
                                 condOutcome == "OLS" &
                                 variable_set_est_CO == "Main")
    
    data.ml <- dplyr::filter(data, propScore == "Logit" &
                               variable_set_est_PS == "Oracle" &
                               condOutcome != "OLS")
    
    info.conv <- dplyr::filter(info, propScore == "Logit" & 
                                 variable_set_est_PS == "Oracle" &
                                 condOutcome == "OLS" &
                                 variable_set_est_CO == "Main")
    
    info.ml <- dplyr::filter(info, propScore == "Logit" &
                               variable_set_est_PS == "Oracle" &
                               condOutcome != "OLS")
    
  } else if (input$misspecification == "COCorrect"){
    
    data.conv <- dplyr::filter(data, propScore == "Logit" & 
                                 variable_set_est_PS == "Main" &
                                 condOutcome == "OLS" &
                                 variable_set_est_CO == "Oracle")
    
    data.ml <- dplyr::filter(data, propScore != "Logit" &
                               condOutcome == "OLS" &
                               variable_set_est_CO == "Oracle")
    
    info.conv <- dplyr::filter(info, propScore == "Logit" & 
                                 variable_set_est_PS == "Main" &
                                 condOutcome == "OLS" &
                                 variable_set_est_CO == "Oracle")
    
    info.ml <- dplyr::filter(info, propScore != "Logit" &
                               condOutcome == "OLS" &
                               variable_set_est_CO == "Oracle")
    
  } else if (input$misspecification == "bothMisspecified"){
    
    data.conv <- dplyr::filter(data, propScore == "Logit" & 
                                 variable_set_est_PS == "Main" &
                                 condOutcome == "OLS" &
                                 variable_set_est_CO == "Main")
    
    data.ml <- dplyr::filter(data, propScore != "Logit" &
                               condOutcome != "OLS" &
                               as.character(propScore) == as.character(condOutcome))
    
    info.conv <- dplyr::filter(info, propScore == "Logit" & 
                                 variable_set_est_PS == "Main" &
                                 condOutcome == "OLS" &
                                 variable_set_est_CO == "Main")
    
    info.ml <- dplyr::filter(info, propScore != "Logit" &
                               condOutcome != "OLS" &
                               as.character(propScore) == as.character(condOutcome))
    
  }
  
} else { # if datatype == real_nsw
  
  data.conv <- dplyr::filter(data, propScore == "Logit" & 
                               variable_set_est_PS == "Main" &
                               condOutcome == "OLS" &
                               variable_set_est_CO == "Main")
  
  data.ml <- dplyr::filter(data, propScore != "Logit" &
                             condOutcome != "OLS" &
                             as.character(propScore) == as.character(condOutcome))
  
  info.conv <- dplyr::filter(info, propScore == "Logit" & 
                               variable_set_est_PS == "Main" &
                               condOutcome == "OLS" &
                               variable_set_est_CO == "Main")
  
  info.ml <- dplyr::filter(info, propScore != "Logit" &
                             condOutcome != "OLS" &
                             as.character(propScore) == as.character(condOutcome))
  
}

data_subset_list <- list(data = data, data.conv = data.conv, data.ml = data.ml,
                         info = info, info.conv = info.conv, info.ml = info.ml)

data_subset_list