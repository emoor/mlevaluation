####################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This function estimates the propensity scores and the conditional outcome means with 
#              the specified methods
#              
#              Input:  data:          outcome variable Y, treatment indicator D, covariates
#                      spec:          specification spec, a row from the specification grid spec_grid
#                      spec_list_all: entire specification list
#                      oracleForms:   true functional forms of Y~X and D~X used to generate data
#
#              Output: ps_cond_data: input data augmented with estimated propensity scores (e.g. Lasso_mdx)
#                                    and conditional outcome means (e.g. Lasso_myd1x and Lasso_myd0x)
#
#####################################################################################################

get_ps_cond_data <- function(data, spec, spec_list_all, oracleForms){
  
  ps_cond_data <- data
  
  # Create grids for different combinations of methods and specifications
  ml_grid <- expand.grid(ml_method = union(spec_list_all$propScore_methods_list[-1], 
                                           spec_list_all$condOutcome_methods_list[-1]), 
                         tuneML = spec_list_all$tune_ML, 
                         Dclassification = spec_list_all$Dclassification)
  
  conv_grid <- expand.grid(variable_set_est = spec_list_all$variable_set_est)
  
  if (spec$datatype == "simulate_diamond1"){
    
    # Controls
    allVars <- c("X.X1", "X.X2", "X.X3", "X.X4", "X.X5", "X.X6", "X.X7", "X.X8", "X.X9", "X.X10")
    contVars <- c("X.X2", "X.X4","X.X7" ,"X.X10")
    dummyVars <- c("X.X1", "X.X3", "X.X5", "X.X6", "X.X8", "X.X9")
    
    form_DX_oracle <- as.formula(paste0("D ~ ", as.character(oracleForms$form_D)[3]))
    form_YX_oracle <- as.formula(paste0("Y ~ ", as.character(oracleForms$form_Y)[3]))
    
  } else if (spec$datatype == "real_nsw"){
    
    # Controls
    allVars <- c("age", "education", "black", "hispanic", "married", "nodegree", "re74", "re75", "u74", "u75")
    contVars <- c("age", "education","re74" ,"re75")
    dummyVars <- c("black", "hispanic", "married", "nodegree", "u74", "u75")
  
  } else if (spec$datatype == "simulate_busso"){
    
    # Controls
    allVars <- c("age", "education", "married", "nodegree", "re74", "re75", "u74", "u75")
    contVars <- c("age", "education","re74" ,"re75")
    dummyVars <- c("married", "nodegree", "u74", "u75")
    
  }
  
  # Create control variable strings
  x <- paste(allVars, collapse = "+")
  x2 <- paste(paste0("I(", contVars,"*", contVars ,")"), collapse = "+") # squared terms
  xinter <- paste0("(", paste(allVars, collapse = "+"), ")^2") # interactions
  xl <- paste(x2, xinter, sep = " + ")
  
  # Formulas
  form_DX_main <- as.formula(paste("D~",paste(x, collapse = "+")))
  form_YX_main <- as.formula(paste("Y~",paste(x, collapse = "+")))
  form_DX_full <- as.formula(paste("D~",paste(xl, collapse = "+")))
  form_YX_full <- as.formula(paste("Y~",paste(xl, collapse = "+")))
  
  # Caret fitControl object
  fitControl <- trainControl(method = "cv", number = 10, verboseIter=FALSE, search="grid", selectionFunction = "best")
  
  # Cross-Fitting
  folds_nb  <- spec$crossFitFolds # number of folds for cross-fitting
  unif_rn   <- runif(n = dim(ps_cond_data)[1])
  group_k   <- as.numeric(cut(unif_rn, quantile(x = unif_rn, probs = seq(0, 1, 1/folds_nb)),include.lowest = TRUE))
  
  # Fit and predict for each fold
  for (f in 1:folds_nb){
    
    folds <- which(group_k == f)
    
    if (folds_nb == 1){
      data.fit  <- ps_cond_data[ folds,]
      data.est  <- ps_cond_data[ folds,]
      
    } else if (folds_nb > 1){
      data.fit  <- ps_cond_data[-folds,]
      data.est  <- ps_cond_data[ folds,]
    } 
    
    data.fit.treated <- data.fit %>% filter(D==1)
    data.fit.control <- data.fit %>% filter(D==0)
    
    data.fit.Dfactor   <- data.fit
    data.fit.Dfactor$D <- factor(data.fit.Dfactor$D)
    
    #############################################################
    # Estimate functions with ML methods
    #############################################################
    
    for(mli in 1:dim(ml_grid)[1]){
      
      if (ml_grid[mli, "ml_method"] == "RF"){
        
        if (ml_grid[mli, "tuneML"] == 0){
          
          if (ml_grid[mli, "Dclassification"] == 0){
            
            model_RF_tML0_Dcl0_mdx <- randomForest(form_DX_main, data = data.fit, ntree=1000)
            ps_cond_data[folds,"RF_tML0_Dcl0_mdx"] <- predict(model_RF_tML0_Dcl0_mdx, newdata = data.est)
            
          } else if (ml_grid[mli, "Dclassification"] == 1){
            
            model_RF_tML0_Dcl1_mdx <- randomForest(form_DX_main, data = data.fit.Dfactor, ntree=1000)
            ps_cond_data[folds,"RF_tML0_Dcl1_mdx"] <- predict(model_RF_tML0_Dcl1_mdx, newdata = data.est, type = "prob")[,2]
            
          }
          
          if (spec$param_of_interest == "ATE"){
            model_RF_tML0_myd1x <- randomForest(form_YX_main, data = data.fit.treated, ntree=1000)
            ps_cond_data[folds,"RF_tML0_myd1x"] <- predict(model_RF_tML0_myd1x, newdata = data.est)
          }
          
          model_RF_tML0_myd0x <- randomForest(form_YX_main, data = data.fit.control, ntree=1000)
          ps_cond_data[folds,"RF_tML0_myd0x"] <- predict(model_RF_tML0_myd0x, newdata = data.est)
          
        } else if (ml_grid[mli, "tuneML"] == 1){
          
          if (ml_grid[mli, "Dclassification"] == 0){
            
            model_RF_tML1_Dcl0_mdx <- train(form_DX_main, data = data.fit, method = "rf", trControl = fitControl, ntree=1000)
            ps_cond_data[folds,"RF_tML1_Dcl0_mdx"] <- predict(model_RF_tML1_Dcl0_mdx, newdata = data.est)
            
          } else if (ml_grid[mli, "Dclassification"] == 1){
            
            model_RF_tML1_Dcl1_mdx <- train(form_DX_main, data = data.fit.Dfactor, method = "rf", trControl = fitControl, ntree=1000)
            ps_cond_data[folds,"RF_tML1_Dcl1_mdx"] <- predict(model_RF_tML1_Dcl1_mdx, newdata = data.est, type = "prob")[,2]
            
          }
          
          if (spec$param_of_interest == "ATE"){
            model_RF_tML1_myd1x <- train(form_YX_main, data = data.fit.treated, method = "rf", trControl = fitControl, ntree=1000)
            ps_cond_data[folds,"RF_tML1_myd1x"] <- predict(model_RF_tML1_myd1x, newdata = data.est)
          }
          
          model_RF_tML1_myd0x <- train(form_YX_main, data = data.fit.control, method = "rf", trControl = fitControl, ntree=1000)
          ps_cond_data[folds,"RF_tML1_myd0x"] <- predict(model_RF_tML1_myd0x, newdata = data.est)
          
        }
        
      } else if (ml_grid[mli, "ml_method"] == "Lasso"){
        
        model_Lasso_mdx <- glmnetUtils::cv.glmnet(form_DX_full, family="binomial", data=data.fit.Dfactor, alpha = 0.5, nfolds=10)
        ps_cond_data[folds,"Lasso_mdx"] <- predict(model_Lasso_mdx, newdata = data.est, s = "lambda.min", type = "response")
        
        if (spec$param_of_interest == "ATE"){
          
          model_Lasso_myd1x <- glmnetUtils::cv.glmnet(form_YX_full, family="gaussian", data=data.fit.treated, alpha = 0.5, nfolds=10)
          ps_cond_data[folds,"Lasso_myd1x"] <- predict(model_Lasso_myd1x, newdata = data.est, s = "lambda.min")
          
        }
        
        model_Lasso_myd0x <- glmnetUtils::cv.glmnet(form_YX_full, family="gaussian", data=data.fit.control, alpha = 0.5, nfolds=10)
        ps_cond_data[folds,"Lasso_myd0x"] <- predict(model_Lasso_myd0x, newdata = data.est, s = "lambda.min")
        
      }
    }
    
    #############################################################
    # Estimate functions with conventional methods
    #############################################################
    
    for(ci in 1:dim(conv_grid)[1]){
      
      if (conv_grid[ci, "variable_set_est"] == "Main"){
        
        model_Logit_Main_mdx <- glm(form_DX_main, family=binomial(link='logit'), data=data.fit)
        ps_cond_data[folds,"Logit_Main_mdx"] <- predict(model_Logit_Main_mdx, newdata = data.est, type = "response")
        
        if (spec$param_of_interest == "ATE"){
          model_OLS_Main_myd1x <- lm(form_YX_main, data = data.fit.treated)
          ps_cond_data[folds,"OLS_Main_myd1x"] <- predict(model_OLS_Main_myd1x, newdata = data.est)
        }
        
        model_OLS_Main_myd0x <- lm(form_YX_main, data = data.fit.control)
        ps_cond_data[folds,"OLS_Main_myd0x"] <- predict(model_OLS_Main_myd0x, newdata = data.est)
        
      } else if (conv_grid[ci, "variable_set_est"] == "Full"){
        
        model_Logit_Full_mdx <- glm(form_DX_full, family=binomial(link='logit'), data=data.fit)
        ps_cond_data[folds,"Logit_Full_mdx"] <- predict(model_Logit_Full_mdx, newdata = data.est, type = "response")
        
        if (spec$param_of_interest == "ATE"){
          model_OLS_Full_myd1x <- lm(form_YX_full, data = data.fit.treated)
          ps_cond_data[folds,"OLS_Full_myd1x"] <- predict(model_OLS_Full_myd1x, newdata = data.est)
        }
        
        model_OLS_Full_myd0x <- lm(form_YX_full, data = data.fit.control)
        ps_cond_data[folds,"OLS_Full_myd0x"] <- predict(model_OLS_Full_myd0x, newdata = data.est)
        
      } else if (conv_grid[ci, "variable_set_est"] == "Oracle"){
        
        model_Logit_Oracle_mdx <- glm(form_DX_oracle, family=binomial(link='logit'), data=data.fit)
        ps_cond_data[folds,"Logit_Oracle_mdx"] <- predict(model_Logit_Oracle_mdx, newdata = data.est, type = "response")
        
        if (spec$param_of_interest == "ATE"){
          model_OLS_Oracle_myd1x <- lm(form_YX_oracle, data = data.fit.treated)
          ps_cond_data[folds,"OLS_Oracle_myd1x"] <- predict(model_OLS_Oracle_myd1x, newdata = data.est)
        }
        
        model_OLS_Oracle_myd0x <- lm(form_YX_oracle, data = data.fit.control)
        ps_cond_data[folds,"OLS_Oracle_myd0x"] <- predict(model_OLS_Oracle_myd0x, newdata = data.est)
        
      }   
      
    }
    
  }
  
  
  return(ps_cond_data)
}