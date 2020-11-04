#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file loads the .RData data file corresponding to the selected datatype input
#
#########################################################################################################

if (input$datatype == "simulate_busso"){
  data <- get(load("Results/data_simulate_busso.RData"))
} else if (input$datatype == "simulate_diamond1"){
  data <- get(load("Results/data_simulate_diamond1.RData"))   
} else if (input$datatype == "real_nsw"){
  data <- get(load("Results/data_real_nsw.RData"))   
}

data <- as.data.frame(data)
data$value <- as.numeric(as.character(data$value))

data
