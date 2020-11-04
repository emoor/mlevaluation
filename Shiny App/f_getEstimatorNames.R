####################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This function formats the estimator names for the output tables
#              
#              Input:  estimator_list: vector of estimator names in short form (e.g. "1NNMwR")
#
#              Output: out: vector of estimator names in formatted long form (e.g. "Matching on PS")
#
#####################################################################################################

getEstimatorNames <- function(estimator_list){
  
  out <- estimator_list
  
  #############################################################################
  if ("SimpleOLS" %in% estimator_list){
    out[which(estimator_list %in% "SimpleOLS")] <- "Simple OLS"
  }
  
  #############################################################################
  if ("Regression" %in% estimator_list){
    out[which(estimator_list %in% "Regression")] <- "Regression"
  }
  
  #############################################################################
  if ("IPW" %in% estimator_list){
    out[which(estimator_list %in% "IPW")] <- "IPW"
  }
  
  #############################################################################
  if ("1NNMwR" %in% estimator_list){
    out[which(estimator_list %in% "1NNMwR")] <- "Matching on PS"
  }
  
  #############################################################################
  if ("BC1NNMwR" %in% estimator_list){
    out[which(estimator_list %in% "BC1NNMwR")] <- "BC Matching on PS"
  }
  
  #############################################################################
  if ("DoubleML" %in% estimator_list){
    out[which(estimator_list %in% "DoubleML")] <- "Doubly Robust"
  }
  
  #############################################################################
  if ("MeanComp" %in% estimator_list){
     out[which(estimator_list %in% "MeanComp")] <- "Difference in Means"
  }
  
  return(out)
}