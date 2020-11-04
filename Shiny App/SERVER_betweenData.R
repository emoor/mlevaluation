#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file extracts the data required to create the between-estimator comparison
#
#########################################################################################################

data_subset_list <- get_data_subset()

data.conv <- data_subset_list$data.conv
data.ml <- data_subset_list$data.ml

data.conv.b <- data.conv %>% 
  dplyr::filter(estimator != "SimpleOLS"  ) %>%
  dplyr::select(estimator, performance, value) %>%
  tidyr::spread(performance, value) %>%
  mutate(MLConv = "Conv")

data.ml.b <- data.ml %>% 
  dplyr::filter(estimator != "SimpleOLS" ) 

if (input$misspecification == "PSCorrect"){
  
  data.ml.b <- data.ml.b %>% 
    dplyr::select(estimator, performance, value, method = condOutcome) %>%
    tidyr::spread(performance, value) %>%
    mutate(MLConv = "ML")
  
} else {
  
  data.ml.b <- data.ml.b %>% 
    dplyr::select(estimator, performance, value, method = propScore) %>%
    tidyr::spread(performance, value) %>%
    mutate(MLConv = "ML")
  
} 

includeOLS <- "SimpleOLS" %in% input$estimators

data.conv.b2 <- union(data.conv.b %>% mutate(method = "Lasso"), data.conv.b %>% mutate(method = "RF"))
data.ml.b$method <- as.character(data.ml.b$method)

data_between <- union(data.conv.b2, data.ml.b)  %>%
  dplyr::select(estimator, method, MLConv, RMSE, RMSE025, RMSE975, RMSPE, MAE, MAE025, MAE975, MAPE, MeanSE, 
                SD, MeanCoef, MeanBias, MedianBias, CR90, CR95)

data_between$estimator <- getEstimatorNames(as.character(data_between$estimator))

if (includeOLS == TRUE) {
  
  data.OLS <- data.conv %>%
    dplyr::filter(estimator == "SimpleOLS"  ) %>%
    dplyr::select(estimator, performance, value) %>%
    tidyr::spread(performance, value) %>%
    mutate(MLConv = "OLS") %>%
    dplyr::select(estimator, MLConv, RMSE, RMSPE, RMSE025, RMSE975, MAE, MAE025, MAE975,
                  MAPE, MeanSE, SD, MeanCoef, MeanBias, MedianBias, CR90, CR95)
  
  data.OLS$estimator <- "Simple OLS"
  data.OLS2 <- union(data.OLS %>% mutate(method = "Lasso"), data.OLS %>% mutate(method = "RF"))
  
  data_between <- union(data_between, data.OLS2) 
  
}

data_between$combName <- paste0(data_between$estimator, " (", data_between$MLConv, ")")

if (input$misspecification == "bothCorrect"){
  data_between <- data_between %>% dplyr::filter(MLConv != "ML" & method == "RF")
}

if (input$misspecification != "bothCorrect" & input$scenarioD == "DA" & input$scenarioY == "YA"){
  data_between <- data_between %>% dplyr::filter(MLConv != "Conv")
}

data_between$method = factor(data_between$method, levels = c("RF", "Lasso"), labels = c("RF", "Elnet")) 
data_between$MLConv = factor(data_between$MLConv, levels = c("ML", "Conv", "OLS"))

data_between_table <- data_between %>%
  mutate(convMLname = ifelse(MLConv == "ML", paste0(MLConv, " (", method, ")"), as.character(MLConv))) %>%
  dplyr::filter(!(MLConv == "Conv" & method == "Lasso")) %>%
  dplyr::select(-combName, -method, -MLConv) %>%
  dplyr::select(estimator, convMLname, MeanBias, SD, RMSE)  %>%
  dplyr::mutate_if(is.numeric, ~round(., 2))

data_between_table <- data_between_table[order(data_between_table$estimator, data_between_table$convMLname),] 

data_between_graph <- data_between %>%
    dplyr::select(estimator, method, MLConv, value=input$performance_graph, MeanBias, SD, combName)

data_between_graph$combName <- factor(data_between_graph$combName, 
                                      levels = subset(data_between_graph, method == "RF")[["combName"]][order(subset(data_between_graph, method == "RF")[["value"]], decreasing = T)])
  
data_between <- list(data_between_table = data_between_table, data_between_graph = data_between_graph)
data_between