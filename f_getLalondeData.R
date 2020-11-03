####################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file loads the Lalonde dataset
#              
#              Input:  nsw_gender:  "men", "women" - > women only psid1/psid2 controls available
#                      nsw_sample:  "Lalonde", "Dehejia", "SmithTodd"
#                      comp_group:  "nsw", "psid1", "psid2", "psid3", "cps1", "cps2", "cps3"
#                      exper_group: "treat", "control": use experimental treatment or control group
#
#              Output: Lalonde dataset data according to input specifications
#
#####################################################################################################

getLalondeData <- function(nsw_gender, nsw_sample, comp_group, exper_group) {
  
  data_path <- "U:/Research/ML Evaluation/Data/"

  ##################################################################################################################
  # MEN 
  ##################################################################################################################
  
  if (nsw_gender=="men"){
  
  # Check if data files already exist: if yes, load data files
    
  if (file.exists(paste0(data_path,"Men/lalonde_men_all.rds"))) {
    
    data_all <- readRDS(paste0(data_path,"Men/lalonde_men_all.rds"))
    
  } else {  # if no, create data files
    
    # Load data obtained from Jeffrey Smith via e-mail (06.05.2019)
    data_smith <- read.dta(paste0(data_path, "Men/original/SmithEarlyRA.dta")) %>%
                      rename(treat = treated, education = educ, hispanic = hisp) %>%
                      dplyr::select(treat, age, education, black, hispanic, married, nodegree, 
                                    re74, re75, re78, dwincl, early_ra, sample)
                           
    # Create subsets of Smith data
    data_smith_lalonde_treated <- filter(data_smith, treat == 1 & sample == 1) %>% 
                                    dplyr::select(-dwincl, -early_ra, -sample) %>%
                                    mutate(s_lalonde = 1, s_dw = 0, s_st = 0, s_cps1 = 0, s_cps2 = 0, 
                                           s_cps3 = 0, s_psid1 = 0, s_psid2 = 0, s_psid3 = 0)
    data_smith_dw_treated      <- filter(data_smith, treat == 1 & sample == 1 & dwincl == 1) %>% 
                                    dplyr::select(-dwincl, -early_ra, -sample) %>%
                                    mutate(s_lalonde = 0, s_dw = 1, s_st = 0, s_cps1 = 0, s_cps2 = 0, 
                                           s_cps3 = 0, s_psid1 = 0, s_psid2 = 0, s_psid3 = 0)
    data_smith_st_treated      <- filter(data_smith, treat == 1 & sample == 1 & early_ra == 1) %>% 
                                    dplyr::select(-dwincl, -early_ra, -sample) %>%
                                    mutate(s_lalonde = 0, s_dw = 0, s_st = 1, s_cps1 = 0, s_cps2 = 0, 
                                           s_cps3 = 0, s_psid1 = 0, s_psid2 = 0, s_psid3 = 0)
    data_smith_lalonde_control <- filter(data_smith, treat == 0 & sample == 1) %>% 
                                    dplyr::select(-dwincl, -early_ra, -sample) %>%
                                    mutate(s_lalonde = 1, s_dw = 0, s_st = 0, s_cps1 = 0, s_cps2 = 0, 
                                           s_cps3 = 0, s_psid1 = 0, s_psid2 = 0, s_psid3 = 0)
    data_smith_dw_control      <- filter(data_smith, treat == 0 & sample == 1 & dwincl == 1) %>% 
                                    dplyr::select(-dwincl, -early_ra, -sample) %>%
                                    mutate(s_lalonde = 0, s_dw = 1, s_st = 0, s_cps1 = 0, s_cps2 = 0, 
                                           s_cps3 = 0, s_psid1 = 0, s_psid2 = 0, s_psid3 = 0)
    data_smith_st_control      <- filter(data_smith, treat == 0 & sample == 1 & early_ra == 1) %>% 
                                    dplyr::select(-dwincl, -early_ra, -sample) %>%
                                    mutate(s_lalonde = 0, s_dw = 0, s_st = 1, s_cps1 = 0, s_cps2 = 0, 
                                           s_cps3 = 0, s_psid1 = 0, s_psid2 = 0, s_psid3 = 0)
    
    # Create subset from Dehejia data
    var_names <- c("treat", "age", "education", "black", "hispanic", "married", "nodegree", "re74", "re75", "re78")
      
    data_cps1_control  <- read.table(paste0(data_path,"Men/original/cps_controls.txt"), col.names = var_names) %>%
                            mutate(s_lalonde = 0, s_dw = 0, s_st = 0, s_cps1 = 1, s_cps2 = 0, 
                                   s_cps3 = 0, s_psid1 = 0, s_psid2 = 0, s_psid3 = 0)
    data_cps2_control  <- read.table(paste0(data_path,"Men/original/cps2_controls.txt"), col.names = var_names) %>%
                            mutate(s_lalonde = 0, s_dw = 0, s_st = 0, s_cps1 = 0, s_cps2 = 1, 
                                   s_cps3 = 0, s_psid1 = 0, s_psid2 = 0, s_psid3 = 0)
    data_cps3_control  <- read.table(paste0(data_path,"Men/original/cps3_controls.txt"), col.names = var_names) %>%
                            mutate(s_lalonde = 0, s_dw = 0, s_st = 0, s_cps1 = 0, s_cps2 = 0, 
                                   s_cps3 = 1, s_psid1 = 0, s_psid2 = 0, s_psid3 = 0)
    data_psid1_control <- read.table(paste0(data_path,"Men/original/psid_controls.txt"), col.names = var_names) %>%
                            mutate(s_lalonde = 0, s_dw = 0, s_st = 0, s_cps1 = 0, s_cps2 = 0, 
                                   s_cps3 = 0, s_psid1 = 1, s_psid2 = 0, s_psid3 = 0)
    data_psid2_control <- read.table(paste0(data_path,"Men/original/psid2_controls.txt"), col.names = var_names) %>%
                            mutate(s_lalonde = 0, s_dw = 0, s_st = 0, s_cps1 = 0, s_cps2 = 0, 
                                   s_cps3 = 0, s_psid1 = 0, s_psid2 = 1, s_psid3 = 0)
    data_psid3_control <- read.table(paste0(data_path,"Men/original/psid3_controls.txt"), col.names = var_names) %>%
                            mutate(s_lalonde = 0, s_dw = 0, s_st = 0, s_cps1 = 0, s_cps2 = 0, 
                                   s_cps3 = 0, s_psid1 = 0, s_psid2 = 0, s_psid3 = 1)
      
      
    # Create combined dataset (since cps2 is a subset of cps1 (and psid2 of psid1 etc.), 
    # some individuals are 2x or 3x included, but below we make sure to select them only once)
    data_all <- bind_rows(data_smith_lalonde_treated, data_smith_dw_treated, data_smith_st_treated,
                          data_smith_lalonde_control, data_smith_dw_control, data_smith_st_control,
                          data_cps1_control, data_cps2_control, data_cps3_control,
                          data_psid1_control, data_psid2_control, data_psid3_control)
  
    # Add u74/u75 variable (indicater whether unemployed in 74 or 75)
    data_all <- mutate(data_all, u74 = as.numeric(re74 == 0), u75 = as.numeric(re75 == 0))
    
    # Save dataset
    saveRDS(data_all, paste0(data_path,"Men/lalonde_men_all.rds"))
    
  }
    
  ##################################################################################################################
  # WOMEN 
  ##################################################################################################################
    
  } else if (nsw_gender=="women"){
    
    if (file.exists(paste0(data_path,"Women/lalonde_women_all.rds"))) {
      
      data_all <- readRDS(paste0(data_path,"Women/lalonde_women_all.rds"))
      
    } else { # if no, create data files
      
      # Load data obtained via https://doi.org/10.1086/692397
      data_smith_cal <- read.dta13(paste0(data_path,"Women/original/NSW_AFDC_CS.dta")) %>%
        rename(treat = treated, education = educ, hispanic = hisp, u74 = zero74, u75 = zero75) %>%
        filter(!is.na(re79) & !is.na(re74) & !is.na(re75) & education >= 0)
    
      # Create subsets of Smith_Cal data
      data_smith_cal_lalonde_treated <- filter(data_smith_cal, sample_lalonde==1 & treat==1) %>% 
                                          mutate(s_lalonde = 1, s_dw = 0, s_st = 0, s_psid1 = 0, s_psid2 = 0)
      data_smith_cal_dw_treated      <- filter(data_smith_cal, sample_dw==1 & treat==1) %>% 
                                          mutate(s_lalonde = 0, s_dw = 1, s_st = 0, s_psid1 = 0, s_psid2 = 0)
      data_smith_cal_st_treated      <- filter(data_smith_cal, sample_early==1 & treat==1) %>% 
                                          mutate(s_lalonde = 0, s_dw = 0, s_st = 1, s_psid1 = 0, s_psid2 = 0)
      data_smith_cal_lalonde_control <- filter(data_smith_cal, sample_lalonde==1 & treat==0) %>% 
                                          mutate(s_lalonde = 1, s_dw = 0, s_st = 0, s_psid1 = 0, s_psid2 = 0)
      data_smith_cal_dw_control      <- filter(data_smith_cal, sample_dw==1 & treat==0) %>% 
                                          mutate(s_lalonde = 0, s_dw = 1, s_st = 0, s_psid1 = 0, s_psid2 = 0)
      data_smith_cal_st_control      <- filter(data_smith_cal, sample_early==1 & treat==0) %>% 
                                          mutate(s_lalonde = 0, s_dw = 0, s_st = 1, s_psid1 = 0, s_psid2 = 0)
      data_smith_cal_psid1           <- filter(data_smith_cal, psid1==1) %>% 
                                          mutate(s_lalonde = 0, s_dw = 0, s_st = 0, s_psid1 = 1, s_psid2 = 0, treat = 0)
      data_smith_cal_psid2           <- filter(data_smith_cal, psid2==1) %>% 
                                          mutate(s_lalonde = 0, s_dw = 0, s_st = 0, s_psid1 = 0, s_psid2 = 1, treat = 0)
      
      # Create combined dataset (since cps2 is a subset of cps1 (and psid2 of psid1 etc.), 
      # some individuals are 2x or 3x included, but below we make sure to select them only once)
      data_all <- bind_rows(data_smith_cal_lalonde_treated, data_smith_cal_dw_treated, data_smith_cal_st_treated,
                            data_smith_cal_lalonde_control, data_smith_cal_dw_control, data_smith_cal_st_control,
                            data_smith_cal_psid1, data_smith_cal_psid2)
      
      # Save dataset
      saveRDS(data_all, paste0(data_path,"Women/lalonde_women_all.rds"))
    
    }
  
  }
  
  if (exper_group == "treat"){
    
    # Select treatment sample from c("Lalonde", "Dehejia", "SmithTodd") 
    if (nsw_sample=="Lalonde") {
      data_treat <- data_all %>% filter(s_lalonde == 1 & treat == 1)
    } else if (nsw_sample=="Dehejia") {
      data_treat <- data_all %>% filter(s_dw == 1 & treat == 1)
    } else if (nsw_sample=="SmithTodd") {
      data_treat <- data_all %>% filter(s_st == 1 & treat == 1)
    } else {
      stop("Loading of NSW treatment data failed in getLalondeData.R!")
    }
    
  } else if (exper_group == "control"){
    
    # Select treatment sample from c("Lalonde", "Dehejia", "SmithTodd") 
    if (nsw_sample=="Lalonde") {
      data_treat0 <- data_all %>% filter(s_lalonde == 1 & treat == 0)
    } else if (nsw_sample=="Dehejia") {
      data_treat0 <- data_all %>% filter(s_dw == 1 & treat == 0)
    } else if (nsw_sample=="SmithTodd") {
      data_treat0 <- data_all %>% filter(s_st == 1 & treat == 0)
    } else {
      stop("Loading of NSW control data failed in getLalondeData.R!")
    }
    
    data_treat <- data_treat0 %>% mutate(treat = 1)
    
  } else {
    stop("Loading of NSW treatment/control data failed in getLalondeData.R!")
  }

  
  # Select control sample from c("nsw", "psid1", "psid2", "psid3", "cps1", "cps2", "cps3")
  if (comp_group=="nsw") {
    data_control <- data_all %>% filter(s_lalonde == 1 & treat == 0)
  } else if (comp_group=="psid1") {
    data_control <- data_all %>% filter(s_psid1 == 1)
  } else if (comp_group=="psid2") {
    data_control <- data_all %>% filter(s_psid2 == 1)
  } else if (comp_group=="psid3" & nsw_gender == "men") {
    data_control <- data_all %>% filter(s_psid3 == 1)
  } else if (comp_group=="cps1" & nsw_gender == "men") {
    data_control <- data_all %>% filter(s_cps1 == 1)
  } else if (comp_group=="cps2" & nsw_gender == "men") {
    data_control <- data_all %>% filter(s_cps2 == 1)
  } else if (comp_group=="cps3" & nsw_gender == "men") {
    data_control <- data_all %>% filter(s_cps3 == 1)
  } else {
    stop("Loading of NSW control data failed in getLalondeData.R! Remember: for women, only NSW/PSID1/PSID2 available!")
  }
  
  # Bind final dataset and drop subsample indicators
  data <- bind_rows(data_treat, data_control) %>%
            dplyr::select(-starts_with("s_"))
  
  return(data)
}
