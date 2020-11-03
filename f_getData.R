####################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file loads the dataset
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

getData <- function(spec){

  if (spec$datatype=="real_nsw"){
    
      if (spec$trimmed_data == 0) {
        
        data_all <- getLalondeData(spec$nsw_gender, spec$nsw_sample, spec$comp_group, spec$exper_group)
        
      } else if (spec$trimmed_data == 1) {
        
        load.string <- paste("MLE_FittedLalonde/data", spec$nsw_gender, spec$nsw_sample, spec$comp_group, 
                             spec$exper_group, "trimmed.RData", sep = "_")
       
        data_all <- NA
        load(load.string)
        
      } else {
        
        stop("trimmed_data in getData.R not specified!")
        
      }
      
      if (spec$draw_bs == 1){ # Draw bootstrap sample if specified
        
        data_all_treated <- data_all %>% dplyr::filter(treat==1)
        data_all_control <- data_all %>% dplyr::filter(treat==0)
        bs_index_treated <- sample(1:dim(data_all_treated)[1], dim(data_all_treated)[1], replace = T)
        bs_index_control <- sample(1:dim(data_all_control)[1], dim(data_all_control)[1], replace = T)
        data_bs_treated <- data_all_treated[bs_index_treated,]
        data_bs_control <- data_all_control[bs_index_control,]
        data <- rbind(data_bs_treated, data_bs_control)
        
      } else if (spec$draw_bs == 0) {
        
        data <- data_all
        
      } else {
        
        stop("draw_bs in getData.R not specified!")
        
      }
      
      if (spec$nsw_gender=="men"){
        
        # Reorder columns of data
        data <- data[c("re78", "treat", colnames(data)[-which(colnames(data) %in% c("ds", "re78","treat"))])]
        colnames(data)[1:2] <- c("Y", "D")
        
        if (spec$exper_group == "treat"){
          
          # Estimate "true" causal effect
          data_nsw <- getLalondeData(spec$nsw_gender, spec$nsw_sample, "nsw", "treat")
          lm.fit <- lm(re78 ~ treat + age + education + black + hispanic + married + nodegree, data = data_nsw)
          
        } 

      } else if (spec$nsw_gender=="women"){
        
        # Reorder columns of data
        data <- data[c("re79", "treat", colnames(data)[-which(colnames(data) %in% c("ds", "re79","treat", "moa", 
                                          "re78", "re77", "re76", "redif", "lalonde", "psid1", "psid2",
                                          "sample_lalonde", "sample_dw", "sample_early"))])]
        colnames(data)[1:2] <- c("Y", "D")
        
        if (spec$exper_group == "treat"){
          
        # Estimate "true" causal effect
          data_nsw <- getLalondeData(spec$nsw_gender, spec$nsw_sample, "nsw", "treat")
          lm.fit <- lm(re79 ~ treat + age + education + black + hispanic + married + nodegree, data = data_nsw)
          
        }
      }
      
      if (spec$exper_group == "treat"){
        
        trueEffect <- coef(lm.fit)[["treat"]]
        
      } else if (spec$exper_group == "control"){
        
        trueEffect <- 0
        
      }
      
      out <- list(data = data, trueEffect = trueEffect, oracleForms = NA, pseudoR2 = NA)
  
    
  } else if (spec$datatype =="simulate_diamond1") {  
    
    out <- sim_mc_diamond1(spec)
    
  } else if (spec$datatype =="simulate_busso") {  
    
    out <- sim_mc_busso(spec)
    
  }

  return(out)
  
}  
  
