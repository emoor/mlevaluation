# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
This is the code repo for my PhD project "Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness". In this project, I analyze the estimation of causal effects using machine learning methods. A description of the project can be found either in the [interactive online appendix](https://eliasmoor.shinyapps.io/mlevaluation/) or in the [paper](https://emoor.github.io/projects/Doctoral_Thesis_EM.pdf#page=18). This repo contains both the data analysis files (Analysis folder) and the Shiny app used to illustrate the results (Shiny App folder).

## Analysis

##### Main file

- mle_main.R: main file calling all analysis files

##### Functions

- f_getData: loads the dataset
- f_sim_mc_diamond1: generates a dataset according to Diamond and Sekon (2013)
- f_sim_mc_busso: draws a sample from the superpopulation according to Busso et al. (2014)
- f_getLalondeData.R: loads the Lalonde dataset
- f_ps_cond_est: estimates the propensity scores and the conditional outcome means with the selected methods
- f_cia_estimators.R: estimates the average treatment effect (ATE/ATT)
- f_saveResults: saves the results from all replications to .RData files (input for Shiny app)

##### Supplementary files

- mle_options.R: loads the required libraries and functions
- create_superPopulation_busso.R: generates a superpopulation according to Busso et al. (2014)

## Shiny App

##### Main file

- app.R: main Shiny app file containing ui and server

##### SERVER files

- SERVER_getData.R: loads the .RData data file corresponding to the selected datatype input
- SERVER_getInfo.R: loads the .RData info file corresponding to the selected datatype input
- SERVER_filterData1.R: filters data and info according to selected user input or specified values
- SERVER_filterData2.R: further filters data and info according to selected user input or specified values
- SERVER_withinData.R: extracts the data required to create the within-estimator comparison
- SERVER_betweenData.R: extracts the data required to create the between-estimator comparison
- SERVER_analysisData.R: extracts the data required to create the analysis comparison mode
- SERVER_histData.R: extracts the data required to create the histogram of propensity scores
- SERVER_makePlot.R: creates the graph for the user selected comparison mode
- SERVER_makeTable.R: creates the table for the user selected comparison mode
- SERVER_makefsTable.R: creates the first stage table displaying the correlations/RMSE of estimated and true propensity scores/conditional
- SERVER_makeHist.R: creates the histogram of propensity scores
- SERVER_makeInfoBottom.R: adds summary information on the selected specification
- SERVER_makeInfoCaptionGraph.R: adds graph captions 
- SERVER_makeInfoCaptionHist.R: adds the histogram caption
- SERVER_makeInfoTitleGraph.R: adds information regarding the user selected comparison mode and misspecification scenario
- SERVER_makeInfoTitleHist.R: adds title to histogram of propensity scores
 outcome means

##### UI files

- UI_Side_DGP.R: defines the "DGP Settings" tab of the UI
- UI_Side_Estimation.R: defines the "Estimation Settings" tab of the UI
- UI_Side_General.R: defines the "General Settings" tab of the UI

##### R Markdown description files

- Background1.Rmd: description of setting and assumptions
- Background2.Rmd: description of structure of estimation procedure, estimation of conditional outcome means and propensity score, estimation treatment effects, performance measures, software
- Data_Diamond1.Rmd: description simulation study based on Diamond and Sekon (2013)
- Data_Sim_Lalonde.Rmd: description simulation study based on Busso et al. (2014)
- Data_realNSW.Rmd: description within-study comparison based on LaLonde (1986)
- Home.Rmd: description app overview
- RO.Rmd: description research objectives

##### Folders

- Folder Results: .RData files with results from analysis
- Folder www: additional material (png figures)


##### Supplementary files

- app_choices.R: stores some app choice options (to retrieve name for graphs)
- f_getEstimatorNames.R: formats the estimator names for the output tables

