#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file extracts the data required to create the analysis comparison mode
#
#########################################################################################################

includeOLS <- "SimpleOLS" %in% input$estimators

data_subset_list1 <- get_data_subset1()

data <- data_subset_list1$data
info <- data_subset_list1$info

analysisDim_temp <- input$analysisDim

if (input$datatype == "simulate_diamond1"){
  
  if ("bothCorrect" %in% input$analysisMisspec) {
    
    data.conv.bothCorrect <- dplyr::filter(data, propScore == "Logit" & 
                                 variable_set_est_PS == "Oracle" &
                                 condOutcome == "OLS" &
                                 variable_set_est_CO == "Oracle") %>%
                                 mutate(misspec = "bothCorrect")
    
    #data.ml will be deleted, i.e. not shown in graph (because ML not possible with misspec == "bothCorrect")
    data.ml.bothCorrect <- dplyr::filter(data, propScore == "Logit" & 
                               variable_set_est_PS == "Oracle" &
                               condOutcome == "OLS" &
                               variable_set_est_CO == "Oracle") %>%
                               mutate(misspec = "bothCorrect")
    
    info.conv.bothCorrect <- dplyr::filter(info, propScore == "Logit" & 
                                 variable_set_est_PS == "Oracle" &
                                 condOutcome == "OLS" &
                                 variable_set_est_CO == "Oracle") %>%
                                 mutate(misspec = "bothCorrect") 
    
    #info.ml will be deleted, i.e. not shown in graph (because ML not possible with misspec == "bothCorrect")
    info.ml.bothCorrect <- dplyr::filter(info, propScore == "Logit" & 
                               variable_set_est_PS == "Oracle" &
                               condOutcome == "OLS" &
                               variable_set_est_CO == "Oracle") %>%
                               mutate(misspec = "bothCorrect")
    
  } 
  
  if ("PSCorrect" %in% input$analysisMisspec) {
    
    data.conv.PSCorrect <- dplyr::filter(data, propScore == "Logit" & 
                                 variable_set_est_PS == "Oracle" &
                                 condOutcome == "OLS" &
                                 variable_set_est_CO == "Main") %>%
                                 mutate(misspec = "PSCorrect")
    
    data.ml.PSCorrect <- dplyr::filter(data, propScore == "Logit" &
                               variable_set_est_PS == "Oracle" &
                               condOutcome != "OLS") %>%
                               mutate(misspec = "PSCorrect")
    
    info.conv.PSCorrect <- dplyr::filter(info, propScore == "Logit" & 
                                 variable_set_est_PS == "Oracle" &
                                 condOutcome == "OLS" &
                                 variable_set_est_CO == "Main") %>%
                                 mutate(misspec = "PSCorrect")
    
    info.ml.PSCorrect <- dplyr::filter(info, propScore == "Logit" &
                               variable_set_est_PS == "Oracle" &
                               condOutcome != "OLS") %>%
                               mutate(misspec = "PSCorrect")
    
  } 
  
  if ("COCorrect" %in% input$analysisMisspec) {
    
    data.conv.COCorrect <- dplyr::filter(data, propScore == "Logit" & 
                                 variable_set_est_PS == "Main" &
                                 condOutcome == "OLS" &
                                 variable_set_est_CO == "Oracle") %>%
                                 mutate(misspec = "COCorrect")
    
    data.ml.COCorrect <- dplyr::filter(data, propScore != "Logit" &
                               condOutcome == "OLS" &
                               variable_set_est_CO == "Oracle") %>%
                               mutate(misspec = "COCorrect")
    
    info.conv.COCorrect <- dplyr::filter(info, propScore == "Logit" & 
                                 variable_set_est_PS == "Main" &
                                 condOutcome == "OLS" &
                                 variable_set_est_CO == "Oracle") %>%
                                 mutate(misspec = "COCorrect")
    
    info.ml.COCorrect <- dplyr::filter(info, propScore != "Logit" &
                               condOutcome == "OLS" &
                               variable_set_est_CO == "Oracle") %>%
                               mutate(misspec = "COCorrect")
    
  } 
  
  if ("bothMisspecified" %in% input$analysisMisspec) {
    
    data.conv.bothMisspecified <- dplyr::filter(data, propScore == "Logit" & 
                                 variable_set_est_PS == "Main" &
                                 condOutcome == "OLS" &
                                 variable_set_est_CO == "Main") %>%
                                  mutate(misspec = "bothMisspecified")
    
    data.ml.bothMisspecified <- dplyr::filter(data, propScore != "Logit" &
                               condOutcome != "OLS" &
                               as.character(propScore) == as.character(condOutcome)) %>%
                               mutate(misspec = "bothMisspecified")
    
    info.conv.bothMisspecified <- dplyr::filter(info, propScore == "Logit" & 
                                 variable_set_est_PS == "Main" &
                                 condOutcome == "OLS" &
                                 variable_set_est_CO == "Main") %>%
                                 mutate(misspec = "bothMisspecified")
    
    info.ml.bothMisspecified <- dplyr::filter(info, propScore != "Logit" &
                               condOutcome != "OLS" &
                               as.character(propScore) == as.character(condOutcome)) %>%
                               mutate(misspec = "bothMisspecified")
    
  }
  
  for (m in 1:length(input$analysisMisspec)) {
    
    if (m == 1) {
      data.conv <- eval(parse(text = paste0("data.conv.", input$analysisMisspec[m])))
      data.ml <- eval(parse(text = paste0("data.ml.", input$analysisMisspec[m])))
      info.conv <- eval(parse(text = paste0("info.conv.", input$analysisMisspec[m])))
      info.ml <- eval(parse(text = paste0("info.ml.", input$analysisMisspec[m])))
    } else {
      data.conv <- rbind(data.conv, eval(parse(text = paste0("data.conv.", input$analysisMisspec[m]))))
      data.ml <- rbind(data.ml, eval(parse(text = paste0("data.ml.", input$analysisMisspec[m]))))
      info.conv <- rbind(info.conv, eval(parse(text = paste0("info.conv.", input$analysisMisspec[m]))))
      info.ml <- rbind(info.ml, eval(parse(text = paste0("info.ml.", input$analysisMisspec[m]))))
    }
    
  }
  
} else { # if datatype == real_nsw or if datatype == simulate_busso
  
  data.conv <- dplyr::filter(data, propScore == "Logit" & 
                               variable_set_est_PS == "Main" &
                               condOutcome == "OLS" &
                               variable_set_est_CO == "Main") %>%
               mutate(misspec = "bothMisspecified")
  
  data.ml <- dplyr::filter(data, propScore != "Logit" &
                             condOutcome != "OLS" &
                             as.character(propScore) == as.character(condOutcome)) %>%
               mutate(misspec = "bothMisspecified")
  
  info.conv <- dplyr::filter(info, propScore == "Logit" & 
                               variable_set_est_PS == "Main" &
                               condOutcome == "OLS" &
                               variable_set_est_CO == "Main") %>%
               mutate(misspec = "bothMisspecified")
  
  info.ml <- dplyr::filter(info, propScore != "Logit" &
                             condOutcome != "OLS" &
                             as.character(propScore) == as.character(condOutcome)) %>%
               mutate(misspec = "bothMisspecified")
  
}

data_subset_list <- list(data = data, data.conv = data.conv, data.ml = data.ml,
                         info = info, info.conv = info.conv, info.ml = info.ml)

data_subset_list


if (input$analysisDim == "linAdd"){ #####################################################
  
  validate({
    
    need("scenarioY" %in% colnames(data.conv),
          "Analysis dimension Linearity/Additivity not available for Empirical Simulation Study and Within-Study Comparison! Please select another analysis dimension!")
  })
  
  data.conv.a <- data.conv %>% 
    dplyr::filter(estimator != "SimpleOLS" &
                    performance == input$performance_graph) %>%
    dplyr::select(estimator, misspec, scenarioY, scenarioD, value) %>%
    mutate(MLConv = "Conv")
  
  data.ml.a <- data.ml %>%
    dplyr::filter(estimator != "SimpleOLS" &
                    performance == input$performance_graph) %>%
    dplyr::select(estimator, misspec, scenarioY, scenarioD, value, condOutcome, propScore) %>%
    mutate(MLConv = "ML")
  
  data.ml.a <- data.ml.a %>%
    mutate(method = ifelse(misspec == "PSCorrect", as.character(condOutcome), as.character(propScore))) %>%
    dplyr::select(-propScore, -condOutcome)
  
  data.conv.a2 <- union(data.conv.a %>% mutate(method = "Lasso"), data.conv.a %>% mutate(method = "RF"))
  data.ml.a$method <- as.character(data.ml.a$method)
  
  data_analysis_allScenario <- union(data.conv.a2, data.ml.a)  
  
  data_analysis_allScenario$estimator <- getEstimatorNames(as.character(data_analysis_allScenario$estimator))
  
  includeOLS <- "SimpleOLS" %in% input$estimators
  
  if (includeOLS == TRUE) {
    
    data.OLS <- data.conv %>% 
      dplyr::filter(estimator == "SimpleOLS" &
                      performance == input$performance_graph ) %>%
      dplyr::select(estimator, misspec, scenarioY, scenarioD, value) %>%
      mutate(MLConv = "OLS")
    
    data.OLS$estimator <- "Simple OLS"
    data.OLS2 <- union(data.OLS %>% mutate(method = "Lasso"), data.OLS %>% mutate(method = "RF"))
    
    data_analysis_allScenario <- union(data_analysis_allScenario, data.OLS2) 
    
  }
  

  if (input$linAddParam == "lin") {
    
    if (input$linAddrelation == "YX") {
      
      Dselector <- c(paste0("D", input$linAddFixed), paste0("D", input$linAddFixed))
      Yselector <- c("YA", "YC")
      
    } else if (input$linAddrelation == "DX") {
      
      Dselector <- c("DA", "DC")
      Yselector <- c(paste0("Y", input$linAddFixed), paste0("Y", input$linAddFixed))
      
    } else if (input$linAddrelation == "DXYX") {
      
      Dselector <- c("DA", "DC")
      Yselector <- c("YA", "YC")
      
    }
    
  } else if (input$linAddParam == "add") {
    
    if (input$linAddrelation == "YX") {
      
      Dselector <- c(paste0("D", input$linAddFixed), paste0("D", input$linAddFixed))
      Yselector <- c("YA", "YF")
      
    } else if (input$linAddrelation == "DX") {
      
      Dselector <- c("DA", "DF")
      Yselector <- c(paste0("Y", input$linAddFixed), paste0("Y", input$linAddFixed))
      
    } else if (input$linAddrelation == "DXYX") {
      
      Dselector <- c("DA", "DF")
      Yselector <- c("YA", "YF")
      
    }
    
  } else if (input$linAddParam == "linAdd") {
    
    if (input$linAddrelation == "YX") {
      
      Dselector <- c(paste0("D", input$linAddFixed), paste0("D", input$linAddFixed))
      Yselector <- c("YA", "YG")
      
    } else if (input$linAddrelation == "DX") {
      
      Dselector <- c("DA", "DG")
      Yselector <- c(paste0("Y", input$linAddFixed), paste0("Y", input$linAddFixed))
      
    } else if (input$linAddrelation == "DXYX") {
      
      Dselector <- c("DA", "DE", "DG")
      Yselector <- c("YA", "YE", "YG")
      
    }
    
  }
    
    data_analysis_allScenario$indicator <- NA

    for (indic in 1:length(Dselector)){
      data_analysis_allScenario$indicator[data_analysis_allScenario$scenarioD == Dselector[indic] &
                                            data_analysis_allScenario$scenarioY == Yselector[indic]] <- indic
    }

    data_analysis1 <-  data_analysis_allScenario %>%
      dplyr::filter(!is.na(indicator))

    data_analysis1$nameY <- data_analysis1$scenarioY
    data_analysis1$nameD <- data_analysis1$scenarioD

    data_analysis1$nameY <- paste0("Y~X: ", str_sub(names(choices_YX_short[match(data_analysis1$nameY, choices_YX_short)]), 4))
    data_analysis1$nameD <- paste0("D~X: ", str_sub(names(choices_DX_short[match(data_analysis1$nameD, choices_DX_short)]), 4))

    data_analysis2 <- data_analysis1 %>%
      mutate(name = paste(nameY, nameD, sep = ", \n "))
    
    data_analysis <- data_analysis2 %>%
      dplyr::select(estimator, misspec, method, MLConv, indicator, value, name)


} else if (input$analysisDim == "tuneML") { ###########################################

  data.ml.a <- data.ml %>%
    dplyr::filter(estimator != "SimpleOLS" &
                    performance == input$performance_graph) %>%
    dplyr::select(estimator, misspec, tune_ML_CO, tune_ML_PS, value, condOutcome, propScore) %>%
    mutate(MLConv = "ML")
  
  data.ml.a <- data.ml.a %>%
    mutate(filter_ind = ifelse(misspec != "bothMisspecified" | ((misspec == "bothMisspecified") & (as.character(tune_ML_CO) == as.character(tune_ML_PS))), "1", "0"),
           method = ifelse(misspec == "PSCorrect", as.character(condOutcome), as.character(propScore)),
           indicator = 1 + as.numeric(ifelse(misspec == "PSCorrect", as.character(tune_ML_CO), as.character(tune_ML_PS)))) %>%
    dplyr::filter(filter_ind == "1") %>%
    dplyr::select(-propScore, -condOutcome, -tune_ML_CO, -tune_ML_PS, -filter_ind)
  
  data_analysis_allScenario <- data.ml.a
  data_analysis_allScenario$estimator <- getEstimatorNames(as.character(data_analysis_allScenario$estimator))
  
  data_analysis1 <-  data_analysis_allScenario %>%
    dplyr::filter(!is.na(indicator)) 
  
  data_analysis2 <- data_analysis1 %>% 
    mutate(name = factor(indicator, levels = c("1", "2"), labels = c("No", "Yes"))) 
  
  data_analysis <- data_analysis2 %>%
    dplyr::select(estimator, misspec, method, MLConv, indicator, value, name)
  
  
} else { # analysisDim not linAdd or tuneML ###########################################
  
  validate({
    
    need(as.character(input$analysisDim) %in% colnames(data.conv),
         "This analysis dimension is not available! Please select another analysis dimension!")
  })
  
  data.conv.a <- data.conv %>% 
    dplyr::filter(estimator != "SimpleOLS" &
                    performance == input$performance_graph) %>%
    dplyr::select(estimator, misspec, as.character(input$analysisDim), value) %>%
    mutate(MLConv = "Conv")
  
  data.ml.a <- data.ml %>%
    dplyr::filter(estimator != "SimpleOLS" &
                    performance == input$performance_graph) %>%
    dplyr::select(estimator, misspec, as.character(input$analysisDim), value, condOutcome, propScore) %>%
    mutate(MLConv = "ML")
  
  data.ml.a <- data.ml.a %>%
    mutate(method = ifelse(misspec == "PSCorrect", as.character(condOutcome), as.character(propScore))) %>%
    dplyr::select(-propScore, -condOutcome)
  
  data.conv.a2 <- union(data.conv.a %>% mutate(method = "Lasso"), data.conv.a %>% mutate(method = "RF"))
  data.ml.a$method <- as.character(data.ml.a$method)
  
  data_analysis_allScenario <- union(data.conv.a2, data.ml.a)  
  
  data_analysis_allScenario$estimator <- getEstimatorNames(as.character(data_analysis_allScenario$estimator))
  
  includeOLS <- "SimpleOLS" %in% input$estimators
  
  if (includeOLS == TRUE) {
    
    data.OLS <- data.conv %>% 
      dplyr::filter(estimator == "SimpleOLS" &
                      performance == input$performance_graph ) %>%
      dplyr::select(estimator, misspec, as.character(input$analysisDim), value) %>%
      mutate(MLConv = "OLS")
    
    data.OLS$estimator <- "Simple OLS"
    data.OLS2 <- union(data.OLS %>% mutate(method = "Lasso"), data.OLS %>% mutate(method = "RF"))
    
    data_analysis_allScenario <- union(data_analysis_allScenario, data.OLS2) 
    
  }
  
  
  uniqueA <- unique(data_analysis_allScenario[[as.character(input$analysisDim)]])
  
  uniqueA_ord <- uniqueA[order(as.numeric(as.character(uniqueA)))]
  
  data_analysis_allScenario$indicator <- NA
  
  for (indic in 1:(length(uniqueA_ord))){
    data_analysis_allScenario$indicator[data_analysis_allScenario[[as.character(input$analysisDim)]] == uniqueA_ord[indic]] <- indic
  }
  
  data_analysis1 <-  data_analysis_allScenario %>%
    dplyr::filter(!is.na(indicator))
  
  data_analysis2 <- data_analysis1 %>% 
    mutate(name = eval(parse(text = as.character(input$analysisDim)))) 
  
  if (input$analysisDim == "Ynoise"){
    
    data_analysis2$name <- names(choices_Ynoise[match(data_analysis2$name, choices_Ynoise)])
    
  } else if (input$analysisDim == "Dclassification_PS"){
    
    data_analysis2$name <- names(choices_Dclassification_PS[match(data_analysis2$name, choices_Dclassification_PS)])
    
  }

  data_analysis <- data_analysis2 %>%
    dplyr::select(estimator, misspec, method, MLConv, indicator, value, name)
  
} ######################### end else for other cases than tuneML and linAdd

data_analysis <- data_analysis %>%
  dplyr::filter(!(MLConv == "ML" & misspec == "bothCorrect"))

data_analysis$estimator = factor(data_analysis$estimator, levels = c("Simple OLS", "Regression", "IPW",
                                                                     "Matching on PS", "BC Matching on PS",
                                                                     "Doubly Robust"))

data_analysis$method = factor(data_analysis$method, levels = c("RF", "Lasso"), labels = c("RF", "Elnet"))
data_analysis$misspec = factor(data_analysis$misspec, levels = c("bothCorrect", "PSCorrect", "COCorrect", "bothMisspecified"),
                                labels = c("PS and CO correct", "PS Correct", "CO Correct", "Both misspecified"))

data_analysis_table <- data_analysis %>%
  mutate(convMLname = ifelse(MLConv == "ML", paste0(MLConv, " (", method, ")"), MLConv)) %>%
  dplyr::filter(!(MLConv == "Conv" & method == "Lasso")) %>%
  dplyr::select(-name, -method, -MLConv) %>%
  dplyr::select(misspec, estimator, convMLname, everything()) %>%
  unique() %>%
  tidyr::spread(indicator, value)

data_analysis_table <- data_analysis_table[order(data_analysis_table$misspec, data_analysis_table$estimator, 
                                                  data_analysis_table$convMLname),]

data_analysis_graph <- data_analysis

data_analysis <- list(data_analysis_table = data_analysis_table, data_analysis_graph = data_analysis_graph)

data_analysis

