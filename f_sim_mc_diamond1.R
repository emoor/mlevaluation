####################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file generates a dataset according to Diamond and Sekon (2013) 
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

sim_mc_diamond1 <- function(spec) {
  
scenarioY          <-   spec[["scenarioY"]]
scenarioD          <-   spec[["scenarioD"]]
n_sim              <-   spec[["n_sim"]]
Ynoise             <-   spec[["Ynoise"]] 
select_into_D      <-   spec[["select_into_D"]]
treat_frac_cons    <-   spec[["treat_frac_cons"]]
  
# Define homogeneous treatment effect
TE <- -0.4

# Draw covariates
X_1 <- rbinom(n = n_sim, size = 1, prob = 0.5)
X_3 <- rbinom(n = n_sim, size = 1, prob = 0.5)
X_5 <- rbinom(n = n_sim, size = 1, prob = 0.5)
X_6 <- rbinom(n = n_sim, size = 1, prob = 0.5)
X_8 <- rbinom(n = n_sim, size = 1, prob = 0.5)
X_9 <- rbinom(n = n_sim, size = 1, prob = 0.5)
X_2 <- rnorm(n = n_sim, mean = 0, sd = 1)
X_4 <- rnorm(n = n_sim, mean = 0, sd = 1)
X_7 <- rnorm(n = n_sim, mean = 0, sd = 1)
X_10 <- rnorm(n = n_sim, mean = 0, sd = 1)

######################################
# Propensity Score Model
#####################################

beta0 <- 0
beta1 <- 0.8
beta2 <- -0.25
beta3 <- 0.6
beta4 <- -0.4
beta5 <- -0.8
beta6 <- -0.5
beta7 <- 0.7

# Nine additivity and linearity scenarios

# A: Additivity and linearity (mean effects only)
lin_pred_DA <- beta0 + beta1*X_1 + beta2*X_2 + beta3*X_3 + beta4*X_4 + beta5*X_5 + beta6*X_6 + beta7*X_7
oracle_form_DA <- as.formula(D ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X5 + X.X6 + X.X7)

# B: Mild non-linearity (one quadratic term)
lin_pred_DB <- beta0 + beta1*X_1 + beta2*X_2 + beta3*X_3 + beta4*X_4 + beta5*X_5 + beta6*X_6 + beta7*X_7 +
                beta2*(X_2^2)
oracle_form_DB <- as.formula(D ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X5 + X.X6 + X.X7 + I(X.X2*X.X2))

# C: Moderate non-linearity (three quadratic term)
lin_pred_DC <- beta0 + beta1*X_1 + beta2*X_2 + beta3*X_3 + beta4*X_4 + beta5*X_5 + beta6*X_6 + beta7*X_7 +
                beta2*(X_2^2) + beta4*(X_4^2) + beta7*(X_7^2)
oracle_form_DC <- as.formula(D ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X5 + X.X6 + X.X7 +
                             I(X.X2*X.X2) + I(X.X4*X.X4) + I(X.X7*X.X7))

# D: Mild non-additivity (four two-way interaction term)
lin_pred_DD <- beta0 + beta1*X_1 + beta2*X_2 + beta3*X_3 + beta4*X_4 + beta5*X_5 + beta6*X_6 + beta7*X_7 +
                beta1*0.5*X_1*X_3 + beta2*0.7*X_2*X_4 + beta4*0.5*X_4*X_5 + beta5*0.5*X_5*X_6
oracle_form_DD <- as.formula(D ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X5 + X.X6 + X.X7 +
                             I(X.X1*X.X3) + I(X.X2*X.X4) + I(X.X4*X.X5) + I(X.X5*X.X6))

# E: Mild non-additivity and non-linearity (three two-way interaction terms and one quadratic term)
lin_pred_DE <- beta0 + beta1*X_1 + beta2*X_2 + beta3*X_3 + beta4*X_4 + beta5*X_5 + beta6*X_6 + beta7*X_7 +
                beta2*(X_2^2) + beta1*0.5*X_1*X_3 + beta2*0.7*X_2*X_4 + beta4*0.5*X_4*X_5 + beta5*0.5*X_5*X_6
oracle_form_DE <- as.formula(D ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X5 + X.X6 + X.X7 +
                               I(X.X2*X.X2) + I(X.X1*X.X3) + I(X.X2*X.X4) + I(X.X4*X.X5) + I(X.X5*X.X6))

# F: Moderate non-additivity (ten two-way interaction terms)
lin_pred_DF <- beta0 + beta1*X_1 + beta2*X_2 + beta3*X_3 + beta4*X_4 + beta5*X_5 + beta6*X_6 + beta7*X_7 +
                beta1*0.5*X_1*X_3 + beta2*0.7*X_2*X_4 + beta3*0.5*X_3*X_5 + beta4*0.7*X_4*X_6 + beta5*0.5*X_5*X_7 +
                beta1*0.5*X_1*X_6 + beta2*0.7*X_2*X_3 + beta3*0.5*X_3*X_4 + beta4*0.5*X_4*X_5 + beta5*0.5*X_5*X_6
oracle_form_DF <- as.formula(D ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X5 + X.X6 + X.X7 +
                               I(X.X1*X.X3) + I(X.X2*X.X4) + I(X.X3*X.X5) + I(X.X4*X.X6) +
                               I(X.X5*X.X7) + I(X.X1*X.X6) + I(X.X2*X.X3) + I(X.X3*X.X4) + 
                               I(X.X4*X.X5) + I(X.X5*X.X6))

# G: Moderate non-additivity and non-linearity (ten two-way interaction terms and three quadratic terms)
lin_pred_DG <- beta0 + beta1*X_1 + beta2*X_2 + beta3*X_3 + beta4*X_4 + beta5*X_5 + beta6*X_6 + beta7*X_7 +
                beta2*(X_2^2) + beta4*(X_4^2) + beta7*(X_7^2) +
                beta1*0.5*X_1*X_3 + beta2*0.7*X_2*X_4 + beta3*0.5*X_3*X_5 + beta4*0.7*X_4*X_6 + beta5*0.5*X_5*X_7 +
                beta1*0.5*X_1*X_6 + beta2*0.7*X_2*X_3 + beta3*0.5*X_3*X_4 + beta4*0.5*X_4*X_5 + beta5*0.5*X_5*X_6
oracle_form_DG <- as.formula(D ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X5 + X.X6 + X.X7 +
                               I(X.X2*X.X2) + I(X.X4*X.X4) + I(X.X7*X.X7) +
                               I(X.X1*X.X3) + I(X.X2*X.X4) + I(X.X3*X.X5) + I(X.X4*X.X6) +
                               I(X.X5*X.X7) + I(X.X1*X.X6) + I(X.X2*X.X3) + I(X.X3*X.X4) + 
                               I(X.X4*X.X5) + I(X.X5*X.X6))

# H: Moderate non-additivity and mild non-linearity (ten two-way interaction terms and one quadratic term)
lin_pred_DH <- beta0 + beta1*X_1 + beta2*X_2 + beta3*X_3 + beta4*X_4 + beta5*X_5 + beta6*X_6 + beta7*X_7 +
               beta2*(X_2^2) +
               beta1*0.5*X_1*X_3 + beta2*0.7*X_2*X_4 + beta3*0.5*X_3*X_5 + beta4*0.7*X_4*X_6 + beta5*0.5*X_5*X_7 +
               beta1*0.5*X_1*X_6 + beta2*0.7*X_2*X_3 + beta3*0.5*X_3*X_4 + beta4*0.5*X_4*X_5 + beta5*0.5*X_5*X_6
oracle_form_DH <- as.formula(D ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X5 + X.X6 + X.X7 +
                               I(X.X2*X.X2) + 
                               I(X.X1*X.X3) + I(X.X2*X.X4) + I(X.X3*X.X5) + I(X.X4*X.X6) +
                               I(X.X5*X.X7) + I(X.X1*X.X6) + I(X.X2*X.X3) + I(X.X3*X.X4) + 
                               I(X.X4*X.X5) + I(X.X5*X.X6))

# I: Mild non-additivity and moderate non-linearity (four two-way interaction terms and three quadratic terms)
lin_pred_DI <- beta0 + beta1*X_1 + beta2*X_2 + beta3*X_3 + beta4*X_4 + beta5*X_5 + beta6*X_6 + beta7*X_7 +
               beta2*(X_2^2) + beta4*(X_4^2) + beta7*(X_7^2) +
               beta1*0.5*X_1*X_3 + beta2*0.7*X_2*X_4 + beta4*0.5*X_4*X_5 + beta5*0.5*X_5*X_6
oracle_form_DI <- as.formula(D ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X5 + X.X6 + X.X7 +
                               I(X.X2*X.X2) + I(X.X4*X.X4) + I(X.X7*X.X7) +
                               I(X.X1*X.X3) + I(X.X2*X.X4) + I(X.X4*X.X5) + I(X.X5*X.X6))

lin_pred_D_string <- paste("lin_pred_", scenarioD, sep ="")
lin_pred_D <- eval(parse(text = lin_pred_D_string))
oracle_D_string <- paste("oracle_form_", scenarioD, sep ="")
oracle_D <- eval(parse(text = oracle_D_string))

# Calculate true propensity score: Logistic transformation
prop_score <- plogis(treat_frac_cons + select_into_D*lin_pred_D)

# Draw treatment indicator
D <- rbinom(n = n_sim, size = 1, prob = prop_score)

# Calculate pseudo R2
X.temp <- cbind(X_1,X_2,X_3,X_4,X_5,X_6,X_7,X_8,X_9,X_10)
colnames(X.temp) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10")
data.temp <- data.frame(D = D, X = X.temp)
PSmodel <- glm(oracle_D, data = data.temp, family = "binomial")
pseudoR2 = 1 - PSmodel$deviance / PSmodel$null.deviance

####################################
# Outcome model
####################################

alpha0 <- -3.85
alpha1 <- 0.3
alpha2 <- -0.36
alpha3 <- -0.73
alpha4 <- -0.2
alpha8 <- 0.71
alpha9 <- -0.19
alpha10 <- 0.26

# Nine additivity and linearity scenarios

# A: Additivity and linearity (mean effects only)
lin_pred_YA <- alpha0 + alpha1*X_1 + alpha2*X_2 + alpha3*X_3 + alpha4*X_4 + alpha8*X_8 + alpha9*X_9 + alpha10*X_10 
oracle_form_YA <- as.formula(Y ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X8 + X.X9 + X.X10)

# B: Mild non-linearity (one quadratic term)
lin_pred_YB <- alpha0 + alpha1*X_1 + alpha2*X_2 + alpha3*X_3 + alpha4*X_4 + alpha8*X_8 + alpha9*X_9 + alpha10*X_10 +
  alpha2*(X_2^2)
oracle_form_YB <- as.formula(Y ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X8 + X.X9 + X.X10 +
                               I(X.X2*X.X2))

# C: Moderate non-linearity (three quadratic term)
lin_pred_YC <- alpha0 + alpha1*X_1 + alpha2*X_2 + alpha3*X_3 + alpha4*X_4 + alpha8*X_8 + alpha9*X_9 + alpha10*X_10 +
  alpha2*(X_2^2) + alpha4*(X_4^2) + alpha10*(X_10^2)
oracle_form_YC <- as.formula(Y ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X8 + X.X9 + X.X10 +
                               I(X.X2*X.X2) + I(X.X4*X.X4) + I(X.X10*X.X10))

# D: Mild non-additivity (four two-way interaction term)
lin_pred_YD <- alpha0 + alpha1*X_1 + alpha2*X_2 + alpha3*X_3 + alpha4*X_4 + alpha8*X_8 + alpha9*X_9 + alpha10*X_10 +
  alpha1*0.5*X_1*X_3 + alpha2*0.7*X_2*X_4 + alpha4*0.5*X_4*X_8 + alpha8*0.5*X_8*X_10
oracle_form_YD <- as.formula(Y ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X8 + X.X9 + X.X10 +
                               I(X.X1*X.X3) + I(X.X2*X.X4) + I(X.X4*X.X8) + I(X.X8*X.X10))

# E: Mild non-additivity and non-linearity (three two-way interaction terms and one quadratic term)
lin_pred_YE <- alpha0 + alpha1*X_1 + alpha2*X_2 + alpha3*X_3 + alpha4*X_4 + alpha8*X_8 + alpha9*X_9 + alpha10*X_10 +
  alpha2*(X_2^2) + alpha1*0.5*X_1*X_3 + alpha2*0.7*X_2*X_4 + alpha4*0.5*X_4*X_9 + alpha8*0.5*X_8*X_10
oracle_form_YE <- as.formula(Y ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X8 + X.X9 + X.X10 +
                               I(X.X2*X.X2) + I(X.X1*X.X3) + I(X.X2*X.X4) + I(X.X4*X.X9) + I(X.X8*X.X10))

# F: Moderate non-additivity (ten two-way interaction terms)
lin_pred_YF <- alpha0 + alpha1*X_1 + alpha2*X_2 + alpha3*X_3 + alpha4*X_4 + alpha8*X_8 + alpha9*X_9 + alpha10*X_10 +
  alpha1*0.5*X_1*X_3 + alpha2*0.7*X_2*X_4 + alpha3*0.5*X_3*X_4 + alpha4*0.7*X_4*X_8 + alpha8*0.5*X_8*X_10 +
  alpha1*0.5*X_1*X_10 + alpha2*0.7*X_2*X_3 + alpha3*0.5*X_3*X_9 + alpha4*0.5*X_4*X_10 + alpha9*0.5*X_9*X_10
oracle_form_YF <- as.formula(Y ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X8 + X.X9 + X.X10 +
                               I(X.X1*X.X3) + I(X.X2*X.X4) + I(X.X3*X.X4) + I(X.X4*X.X8) + 
                               I(X.X8*X.X10) + I(X.X1*X.X10) + I(X.X2*X.X3) + I(X.X3*X.X9) + 
                               I(X.X4*X.X10) + I(X.X9*X.X10))

# G: Moderate non-additivity and non-linearity (ten two-way interaction terms and three quadratic terms)
lin_pred_YG <- alpha0 + alpha1*X_1 + alpha2*X_2 + alpha3*X_3 + alpha4*X_4 + alpha8*X_8 + alpha9*X_9 + alpha10*X_10 +
  alpha2*(X_2^2) + alpha4*(X_4^2) + alpha10*(X_10^2) +
  alpha1*0.5*X_1*X_3 + alpha2*0.7*X_2*X_4 + alpha3*0.5*X_3*X_4 + alpha4*0.7*X_4*X_8 + alpha8*0.5*X_8*X_10 +
  alpha1*0.5*X_1*X_10 + alpha2*0.7*X_2*X_3 + alpha3*0.5*X_3*X_9 + alpha4*0.5*X_4*X_10 + alpha9*0.5*X_9*X_10
oracle_form_YG <- as.formula(Y ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X8 + X.X9 + X.X10 +
                               I(X.X2*X.X2) + I(X.X4*X.X4) + I(X.X10*X.X10) +
                               I(X.X1*X.X3) + I(X.X2*X.X4) + I(X.X3*X.X4) + I(X.X4*X.X8) + 
                               I(X.X8*X.X10) + I(X.X1*X.X10) + I(X.X2*X.X3) + I(X.X3*X.X9) + 
                               I(X.X4*X.X10) + I(X.X9*X.X10))

# H: Moderate non-additivity and mild non-linearity (ten two-way interaction terms and one quadratic term)
lin_pred_YH <- alpha0 + alpha1*X_1 + alpha2*X_2 + alpha3*X_3 + alpha4*X_4 + alpha8*X_8 + alpha9*X_9 + alpha10*X_10 +
  alpha2*(X_2^2) +
  alpha1*0.5*X_1*X_3 + alpha2*0.7*X_2*X_4 + alpha3*0.5*X_3*X_4 + alpha4*0.7*X_4*X_8 + alpha8*0.5*X_8*X_10 +
  alpha1*0.5*X_1*X_10 + alpha2*0.7*X_2*X_3 + alpha3*0.5*X_3*X_9 + alpha4*0.5*X_4*X_10 + alpha9*0.5*X_9*X_10
oracle_form_YH <- as.formula(Y ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X8 + X.X9 + X.X10 +
                               I(X.X2*X.X2) + 
                               I(X.X1*X.X3) + I(X.X2*X.X4) + I(X.X3*X.X4) + I(X.X4*X.X8) + 
                               I(X.X8*X.X10) + I(X.X1*X.X10) + I(X.X2*X.X3) + I(X.X3*X.X9) + 
                               I(X.X4*X.X10) + I(X.X9*X.X10))

# I: Mild non-additivity and moderate non-linearity (four two-way interaction terms and three quadratic terms)
lin_pred_YI <- alpha0 + alpha1*X_1 + alpha2*X_2 + alpha3*X_3 + alpha4*X_4 + alpha8*X_8 + alpha9*X_9 + alpha10*X_10 +
  alpha2*(X_2^2) + alpha4*(X_4^2) + alpha10*(X_10^2) +
  alpha1*0.5*X_1*X_3 + alpha2*0.7*X_2*X_4 +  alpha4*0.5*X_4*X_8 + alpha8*0.5*X_8*X_10 
oracle_form_YI <- as.formula(Y ~ X.X1 + X.X2 + X.X3 + X.X4 + X.X8 + X.X9 + X.X10 +
                               I(X.X2*X.X2) + I(X.X4*X.X4) + I(X.X10*X.X10) +
                               I(X.X1*X.X3) + I(X.X2*X.X4) + I(X.X4*X.X8) + I(X.X8*X.X10))

# Calculate "linear" index  
lin_pred_Y_string <- paste("lin_pred_", scenarioY, sep ="")
Y_linInd <- eval(parse(text = lin_pred_Y_string))  

# Calculate variance of error term such that R2 approx. equals pseudo R2 from PS model
vareps <- ((1-pseudoR2)/pseudoR2)*var(Y_linInd)

# Calculate outcome 
Y0 <- Y_linInd + Ynoise*rnorm(n = n_sim, mean = 0, sd = sqrt(vareps))
Y1 <- D*TE + Y0
Y <- D*Y1 + (1-D)*Y0

oracle_Y_string <- paste("oracle_form_", scenarioY, sep ="")
oracle_Y <- eval(parse(text = oracle_Y_string))

X <- cbind(X_1,X_2,X_3,X_4,X_5,X_6,X_7,X_8,X_9,X_10)
colnames(X) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10")

data <- data.frame(Y = Y, D = D, Y1 = Y1, Y0 = Y0, truePS = prop_score, X = X)

# Return true effect
trueEffect <- TE

# List with DGP forms
oracleForms <- list(form_D = oracle_D, form_Y = oracle_Y)

out <- list(data = data, trueEffect = trueEffect, oracleForms = oracleForms, pseudoR2 = pseudoR2)

return(out)
}