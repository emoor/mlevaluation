####################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file draws a sample from the superpopulation according to Busso et al. (2014)
#              
#              Input:  specification spec, a row from the specification grid spec_grid
#
#              Output: list out containing the following elements
#
#                      data:        outcome variable Y, treatment indicator D, covariates
#                      trueEffect:  true causal effect
#                      oracleForms: true functional forms of Y~X and D~X used to generate data
#                      pseudoR2:    pseudo R2 from D~X
#
#####################################################################################################

sim_mc_busso <- function(spec) {

n_sim              <-   spec[["n_sim"]]
select_into_D      <-   spec[["select_into_D"]] 
treat_frac_cons    <-   spec[["treat_frac_cons"]]
Ynoise             <-   spec[["Ynoise"]] 

nsw_sample_fit <- "Dehejia"
comp_group_fit <- "psid1"

# Superpopulation identifier
superPopString <- paste("superPop", spec$datatype, nsw_sample_fit, comp_group_fit,
                        str_replace(as.character(select_into_D), "\\.",""), 
                        str_replace(as.character(treat_frac_cons), "\\.",""), 
                        str_replace(as.character(Ynoise), "\\.",""), sep = "_")
  
if (file.exists(paste0("MLE_FittedLalonde/Superpopulations/", superPopString, ".RData"))){
  
  load(paste0("MLE_FittedLalonde/Superpopulations/", superPopString, ".RData"))
  
} else {
  
  stop("No Superpopulation available for this specification. Create superpopulation first!")
  
} 

# Draw sample from superpopulation
sample.ind <- sample(1:dim(data.superPop)[1], n_sim, replace = F)
data <- data.superPop[sample.ind,]


if (param_of_interest=="ATT"){
  
  trueEffect <- ATT.super
  
} else if (param_of_interest=="ATE"){
  
  trueEffect <- ATE.super
  
}

out <- list(data = data, trueEffect = trueEffect, oracleForms = NA, pseudoR2 = NA)

return(out)
}
