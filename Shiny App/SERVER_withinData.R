#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file extracts the data required to create the within-estimator comparison
#
#########################################################################################################

data_subset_list <- get_data_subset()
data.conv <- data_subset_list$data.conv
data.ml <- data_subset_list$data.ml

data.conv.w <- data.conv %>% 
  dplyr::filter(performance == input$performance_graph  &
                  estimator        != "SimpleOLS"  ) %>%
  dplyr::select(estimator, performance, value)

data.ml.w <- data.ml %>% 
  dplyr::filter(performance == input$performance_graph  &
                  estimator        != "SimpleOLS" )

if (input$misspecification == "PSCorrect"){
  
  data.ml.w <- data.ml.w %>% 
    dplyr::select(estimator, performance, value, method = condOutcome)
  
} else {
  
  data.ml.w <- data.ml.w %>% 
    dplyr::select(estimator, performance, value, method = propScore)
  
} 

# Ratio table
data_within <- inner_join(data.conv.w, data.ml.w, by = c("estimator", "performance"), suffix = c(".conv", ".ml"))  %>%
  mutate(relChange = round((value.ml - value.conv)/value.conv,2))

data_within[is.na(data_within$relChange),"relChange"] <- 0

data_within$estimator <- getEstimatorNames(as.character(data_within$estimator))

data_within$method = factor(data_within$method, levels = c("RF", "Lasso"), labels = c("RF", "Elnet")) 

data_within$estimator <- factor(data_within$estimator, 
     levels = subset(data_within, method == "RF")[["estimator"]][order(subset(data_within, method == "RF")[["relChange"]], decreasing = F)])

if (input$misspecification == "bothCorrect"){
  data_within <- data_within %>% mutate(value.conv = 0, value.ml = 0, relChange = 0)
}

if (input$misspecification != "bothCorrect" & input$scenarioD == "DA" & input$scenarioY == "YA"){
  data_within <- data_within %>% mutate(value.conv = 0, value.ml = 0, relChange = 0)
}

data_within_table <- data_within %>% 
  dplyr::select(estimator, method, performance, value.conv, value.ml, relChange)

data_within_graph <- data_within

data_within_graph$estimator <- factor(data_within_graph$estimator, 
  levels = subset(data_within_graph, method == "RF")[["estimator"]][order(subset(data_within_graph, method == "RF")[["relChange"]], decreasing = T)])

data_within <- list(data_within_table = data_within_table, data_within_graph = data_within_graph)
data_within