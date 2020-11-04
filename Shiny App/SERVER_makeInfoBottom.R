#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file adds summary information on the selected specification
#
#########################################################################################################

data_subset_list <- get_data_subset()

info.conv <- data_subset_list$info.conv

if (input$comparison_mode != "analysis"){
  html_text11 <- paste0("<b> Summary of shown specification:</b> (means over ", 
                       data_subset_list$info.conv$reps, " replications) <br> 
                       True TE: ", round(info.conv$true_effect, 2),
                       ", Fraction Treated: ", round(info.conv$treated_fraction, 2),
                       ", #Obs Fitting P(X): ", round(info.conv$n_dx, 2),
                       ", #Obs Estimation TE (after trim conv ps): ", round(info.conv$n_trim, 0),
                       ", #Obs Fitting E(Y|X,D=0): ", round(info.conv$n_dy0x, 0))
  
  html_text12 <- paste0(", #Obs Fitting E(Y|X,D=1): ", round(info.conv$n_dy1x, 0))
  
  if (input$datatype == "simulate_diamond1") {
    html_text1 <- HTML(paste0(html_text11, html_text12))
  } else if (input$datatype == "simulate_busso" | input$datatype == "real_nsw") {
    html_text1 <- HTML(html_text11)
  }
  
} else {
  html_text1 <- HTML(paste0(" "))
}

if (input$comparison_mode != "analysis" & input$datatype == "simulate_busso"){
  html_text2 <- HTML(paste0("Sample Splits: ", info.conv$sampleSplits,
                           ", Cross-Fit Folds: ", info.conv$crossFitFolds,
                           ", PS: ", info.conv$propScore,
                           ", PS Treated Conv: ", round(info.conv$mean_ps_treated, 2),
                           ", PS Control Conv: ", round(info.conv$mean_ps_control, 2),
                           ", CO: ", info.conv$condOutcome,
                           ", NSim: ", info.conv$n_sim,
                           ", Select into D: ", info.conv$select_into_D

  ))
} else if (input$comparison_mode != "analysis" & input$datatype == "simulate_diamond1"){
  html_text2 <- HTML(paste0("Sample Splits: ", info.conv$sampleSplits,
                           ", Cross-Fit Folds: ", info.conv$crossFitFolds,
                           ", PS: ", info.conv$propScore,
                           ", PS Treated Conv: ", round(info.conv$mean_ps_treated, 2),
                           ", PS Control Conv: ", round(info.conv$mean_ps_control, 2),
                           ", CO: ", info.conv$condOutcome,
                           ", ScenarioY: ", info.conv$scenarioY,
                           ", ScenarioD: ", info.conv$scenarioD,
                           ", NSim: ", info.conv$n_sim,
                           ", Select into D: ", info.conv$select_into_D
  ))
} else {
  html_text2 <- HTML(paste0(""))
}

html_text <- HTML(paste0(html_text1, "<br>", html_text2))

html_text