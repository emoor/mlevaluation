# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
This is the code repo for my PhD project "Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness". In this project, I analyze the estimation of causal effects using machine learning methods. A description of the project can be found either in the [interactive online appendix](https://eliasmoor.shinyapps.io/mlevaluation/) or in the [paper](https://emoor.github.io/projects/Doctoral_Thesis_EM.pdf#page=18). This repo contains both the analysis files and the Shiny app used to illustrate the results.

## Overview of code files

### Analysis

- mle_main.R: main file calling all analysis files
- mle_options.R: loads the required libraries and functions
- create_superPopulation_busso.R: generates a superpopulation according to Busso et al. (2014)
- f_cia_estimators.R: estimates the average treatment effect (ATE/ATT)
- f_getData: loads the dataset
- f_getLalondeData.R: loads the Lalonde dataset
- f_ps_cond_est: estimates the propensity scores and the conditional outcome means with the selected methods
- f_saveResults: saves the results from all replications to .RData files (input for Shiny app)
- f_sim_mc_busso: draws a sample from the superpopulation according to Busso et al. (2014)
- f_sim_mc_diamond1: generates a dataset according to Diamond and Sekon (2013)

### Shiny App
