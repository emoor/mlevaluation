#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file adds information regarding the user selected comparison mode and misspecification scenario
#
#########################################################################################################

 if (input$comparison_mode == "within") {
   
   html_text1 <- paste0("<b>Within-Estimator Comparison for</b> ")
   
 } else if (input$comparison_mode == "between") {
   
   html_text1 <- paste0("<b>Between-Estimator Comparison for</b> ")
   
 } else {
   
   html_text1 <- paste0("<b>Analysis</b> ")
   
 }
 
 if (input$misspecification == "bothCorrect") {
   
   html_text2 <- paste0("<b>Misspecification Scenario I (Both PS and Cond. Outcome correctly specified)</b>")
   
 } else if (input$misspecification == "PSCorrect") {
   
   html_text2 <- paste0("<b>Misspecification Scenario II (PS correctly specified, Cond. Outcome misspecified)</b>")
   
 } else if (input$misspecification == "COCorrect") {
   
   html_text2 <- paste0("<b>Misspecification Scenario III (Cond. Outcome correctly specified, PS misspecified)</b>")
   
 } else {
   
   html_text2 <- paste0("<b>Misspecification Scenario IV (Both PS and Cond. Outcome misspecified)</b>")
   
 }
  
  if (input$comparison_mode != "analysis") {
    
    html_text <- HTML(paste0(html_text1, html_text2))
    
  } else {
    
    html_text <- HTML(paste0(html_text1))
    
  }
  
 