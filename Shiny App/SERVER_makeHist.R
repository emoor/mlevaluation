#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file creates the histogram of propensity scores
#
#########################################################################################################

hist_data <- get_hist_data()

if (input$misspecification == "bothCorrect"){
  hist_data <- hist_data %>% dplyr::filter(mlConv != "ML")
}

if (input$misspecification != "bothCorrect" & input$scenarioD == "DA" & input$scenarioY == "YA"){
  hist_data <- hist_data %>% dplyr::filter(mlConv != "Conv")
}

mlConvVec <- unique(hist_data$mlConv)

hist_data_true_single <- hist_data %>% dplyr::filter(mlConv == "True")

for (li in 1:length(mlConvVec)){
  if (li == 1){
    hist_data_true <- hist_data_true_single %>% mutate(mlConv = mlConvVec[li])
  } else {
    hist_data_true <- rbind(hist_data_true, (hist_data_true_single %>% mutate(mlConv = mlConvVec[li])))
  }
}

hist_data_true <- hist_data_true %>% dplyr::filter(mlConv != "True")
hist_data <- hist_data %>% dplyr::filter(mlConv != "True")

mle_hist <- ggplot(hist_data, aes(x = bin, y = value, fill = group, color = group)) +
  facet_grid(. ~ mlConv) +
  scale_color_grey(start = 0.4, end = 0.7) +
  scale_fill_grey(start = 0.4, end = 0.7) +
  labs(y = "Frequency", x = "Propensity Score")

mle_hist
