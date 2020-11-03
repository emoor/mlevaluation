####################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file generates a superpopulation according to Busso et al. (2014)
#              and saves it under MLE_FittedLalonde/Superpopulations. The idea is 
#              to fit parametric distributions to the Lalonde dataset and subsequently 
#              generate a population from the fitted distributions
#
#####################################################################################################

# Clear memory
rm(list = ls())

# Set working directory ####
setwd("U:/Research/ML Evaluation/R_MLE")

# Load packages and functions, set options
source('mle_options.R')

# Number of observations in superpopulation
n_super  <- 1000000

set.seed(1989)

# Specifications of superpopulation
select_into_D <- 1
treat_frac_cons <- 0
Ynoise <- 1
datatype <- "simulate_busso"

# Set Lalonde specification
nsw_sample_fit <- "Dehejia"
comp_group_fit <- "psid1"

# Superpopulation identifier
superPopString <- paste("superPop", datatype, nsw_sample_fit, comp_group_fit,
                        str_replace(as.character(select_into_D), "\\.",""), 
                        str_replace(as.character(treat_frac_cons), "\\.",""), 
                        str_replace(as.character(Ynoise), "\\.",""), sep = "_")

# Load original Lalonde data
data_psid1 <- getLalondeData(nsw_gender = "men", nsw_sample = nsw_sample_fit,
                             comp_group = comp_group_fit, exper_group = "treat")

# Convert to data.frame
data_psid1 <- data.frame(data_psid1)

colnames_data <- colnames(data_psid1)

# Generate and mutate some variables
data_psid1 <- data_psid1 %>% 
  mutate(re74 = re74/1000, re75 = re75/1000, re78 = re78/1000,
         u74 = as.numeric((re74==0)), u75 = as.numeric((re75==0))) %>% 
  filter(black==1)

#############################################################
# Draw covariates
#############################################################

# Draw married, nodegree, u74 and u75
married <- rbinom(n_super, size = 1, prob = mean(data_psid1$married))
nodegree <- rbinom(n_super, size = 1, prob = mean(data_psid1$nodegree))
u74 <- rbinom(n_super, size = 1, prob = mean(data_psid1$u74))
u75 <- rbinom(n_super, size = 1, prob = mean(data_psid1$u75))

# Prepare dataset
dataset <- data.frame(married, nodegree, u74, u75)
dataset <- cbind(dataset, age = NA, education = NA, re74 = NA, re75 = NA)

# Draw age, education, re74 and re75 from conditional multivariate normal
for (married_i in 0:1){
  for (nodegree_i in 0:1){
    for (u74_i in 0:1){
      for (u75_i in 0:1){
        
        data_subset <- subset(data_psid1, married == married_i & nodegree == nodegree_i & u74 == u74_i & u75 == u75_i)
        print(dim(data_subset)[1])
        
        data_subset_simul_ind <- (dataset$married == married_i & 
                                  dataset$nodegree == nodegree_i & 
                                  dataset$u74 == u74_i & 
                                  dataset$u75 == u75_i)
        n_subset <- sum(data_subset_simul_ind)
        
        if (n_subset>0){
          varcov_mat <- cov(data.frame(data_subset$age, data_subset$education, data_subset$re74, data_subset$re75))
          mean_mat <- sapply(data.frame(data_subset$age, data_subset$education, data_subset$re74, data_subset$re75), mean)
          min74 <- min(data_subset$re74)
          min75 <- min(data_subset$re75)
          max74 <- max(data_subset$re74)
          max75 <- max(data_subset$re75)
          
          dataset[data_subset_simul_ind, 5:8] <- rmvnorm(n_subset, mean = mean_mat, sigma = varcov_mat)
          dataset[data_subset_simul_ind, 5:6] <- round(dataset[data_subset_simul_ind, 5:6],0)
          
          minmax_mat <- data.frame(min74 = min74, re74 = dataset[data_subset_simul_ind, 7], max74 = max74,
                                   min75 = min75, re75 = dataset[data_subset_simul_ind, 8], max75 = max75)
          
          dataset[data_subset_simul_ind, 7] <- pmin(pmax(minmax_mat$min74,minmax_mat$re74),minmax_mat$max74)
          dataset[data_subset_simul_ind, 8] <- pmin(pmax(minmax_mat$min75,minmax_mat$re75),minmax_mat$max75)
          
        }
      }
    }
  }
}


#############################################################
# Draw logistic errors U_i
#############################################################

U_i <- rlogis(n_super)

#############################################################
# Construct treatment indicator D and outcome Y
#############################################################

X <- "age + education + nodegree + married + u74 + u75 + re74 + re75"
Z <- "age + education + nodegree + married + u74 + u75 + re74 + re75 + I(re74*re74) + I(re75*re75) + I(u74*u75) + I(re74*re75)"

# Estimate logit on NSW sample
form.logit <- as.formula(paste("treat~", Z))
fit.logit <- glm(form.logit, family=binomial(link='logit'), data=data_psid1)

# Calculate linear index of Tstar
Tstar_lin <- (treat_frac_cons + select_into_D*predict(fit.logit, newdata = dataset))

# Calculate true propensity score
T_prob <- plogis(Tstar_lin)

# Create T*
Tstar <- Tstar_lin - U_i

# Create T
D <- as.numeric(Tstar>0)

# Estimate regression on NSW control sample
form.lm.control <- as.formula(paste("re78~", Z))
fit.lm.control <- lm(form.lm.control, data=subset(data_psid1, treat==0))
sigma_0 <- sqrt(mean((fit.lm.control$residuals)^2))

# Estimate regression on NSW treated sample
form.lm.treated <- as.formula(paste("re78~", Z))
fit.lm.treated <- lm(form.lm.treated, data=subset(data_psid1, treat==1))
sigma_1 <- sqrt(mean((fit.lm.treated$residuals)^2))

# Draw eps0
eps0 <- rnorm(n_super, mean = 0, sd = sigma_0)

# Construct Y0
Y0 <- predict(fit.lm.control, newdata = dataset) + eps0

# Draw eps1
eps1 <- rnorm(n_super, mean = 0, sd = sigma_1)

# Construct Y1
Y1 <- predict(fit.lm.treated, newdata = dataset) + eps1

# Construct observed outcome
Y <- D*Y1 + (1-D)*Y0 


################################################################################
# Define outcome, treatment and covariates 
################################################################################

data <- data.frame(Y = Y, D = D, Y1 = Y1, Y0 = Y0, truePS = T_prob, X = as.matrix(dataset))

################################################################################
# Delete X in front of covariates
################################################################################

colnames(data) <- str_replace(colnames(data), "X.", "")

################################################################################
# TRUE ATE/ATT
################################################################################

data_treated <- data %>% dplyr::filter(D==1)

ATE.super <- mean(data$Y1 - data$Y0)
ATT.super <- mean(data_treated$Y1 - data_treated$Y0)
data.superPop <- data

# Save superpopulation file
save(data.superPop, ATE.super, ATT.super, file = paste0("MLE_FittedLalonde/Superpopulations/", superPopString, ".RData"))

# Print some information on the generated superpopulation
print(paste0("ATT: ", ATT.super))
print(paste0("ATE: ", ATE.super))
print(paste0("Frac larger 0.99: ", mean(data.superPop$truePS>0.99)))
print(paste0("Frac larger 0.95: ", mean(data.superPop$truePS>0.95)))
print(paste0("Frac larger 0.90: ", mean(data.superPop$truePS>0.9)))
print(paste0("Max true PS: ", max(data.superPop$truePS)))
print(paste0("Expected number of treated in 100 sampled obs: ", mean(data.superPop$D)*100))
print(paste0("Number of obs superpopulaton: ", dim(data.superPop)[1]))
hist(subset(data.superPop, D == 1)$truePS)
hist(subset(data.superPop, D == 0)$truePS)
hist(data.superPop$truePS)

# Send pushover notification to smartphone: superpopulation generated
pushover(message=paste0("MLE create_superPopulation.R code run finished."), title="R @ RDSX")

