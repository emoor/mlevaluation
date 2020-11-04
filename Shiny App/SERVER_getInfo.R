#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file loads the .RData info file corresponding to the selected datatype input
#
#########################################################################################################

if (input$datatype == "simulate_busso"){
  
  info <- get(load("Results/hist_simulate_busso.RData"))
  
} else if (input$datatype == "simulate_diamond1"){
  
  info <- get(load("Results/hist_simulate_diamond1.RData"))  
  
} else if (input$datatype == "real_nsw"){
  
  info <- get(load("Results/hist_real_nsw.RData"))   
  
}

info <- as.data.frame(info)

indx <- startsWith(colnames(info), "hist") | startsWith(colnames(info), "treated") | 
  startsWith(colnames(info), "mean") | startsWith(colnames(info), "timestamp") | 
  startsWith(colnames(info), "n_")   | startsWith(colnames(info), "true")

info[indx] <- lapply(info[indx], function(x) as.numeric(as.character(x)))

info