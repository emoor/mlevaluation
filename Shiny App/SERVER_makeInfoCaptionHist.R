#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file adds the histogram caption
#
#########################################################################################################

if (input$misspecification == "bothCorrect") {
  
  text <- paste0("misspecification scenario I (both PS and CO correctly specified)")
  
} else if (input$misspecification == "PSCorrect") {
  
  text <- paste0("misspecification scenario II (PS correctly specified, CO misspecified)")
  
} else if (input$misspecification == "COCorrect") {
  
  text <- paste0("misspecification scenario III (CO correctly specified, PS misspecified)")
  
} else {
  
  text <- paste0("misspecification scenario IV (Both PS and CO misspecified)")
  
}

html_text <- HTML(paste0("Note: The graph shows the median (over 5000 simulation replications) histogram of estimated propensity scores by logistic regression (Logit), random forests (RF), and elastic net (Elnet) for the ",
text, ". The darker gray bars (line) represent the distribution of the estimated (true) propensity scores of control observations, the lighter gray bars (line) represent the distribution of the estimated (true) propensity scores of treated observations."))