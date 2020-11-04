#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file defines the "DGP Settings" tab of the UI
#
#########################################################################################################

list(
  
conditionalPanel(condition = "input.datatype == 'simulate_diamond1'", ##############################################
                 
                 conditionalPanel(condition = "input.comparison_mode != 'analysis' | (input.comparison_mode == 'analysis' & 
                 input.analysisDim != 'linAdd')", 
                                  
                                  # Select D~X relationship
                                  selectInput(inputId  = "scenarioD", 
                                              label    = "D~X relationship:",
                                              choices  = choices_DX, 
                                              selected = "DG"),
                                  
                                  # Select Y~X relationship
                                  selectInput(inputId  = "scenarioY", 
                                              label    = "Y~X relationship:",
                                              choices  = choices_YX, 
                                              selected = "YG"))               
),

conditionalPanel(condition = "input.datatype == 'simulate_diamond1' | input.datatype == 'simulate_busso'", #######################
                    
conditionalPanel(condition = "input.comparison_mode != 'analysis' | (input.comparison_mode == 'analysis' & 
                 input.analysisDim != 'n_sim')", 
                 
   # Select number of observations
   selectInput(inputId  = "n_sim", 
               label    = "Number of Observations:",
               choices  = c("250"  = "250",
                            "500" = "500",
                            "1000" = "1000",
                            "2000" = "2000",
                            "5000" = "5000"), 
               selected = "1000")),

conditionalPanel(condition = "input.comparison_mode != 'analysis' | (input.comparison_mode == 'analysis' & 
                 input.analysisDim != 'select_into_D')", 
                 
                 # Select selection into treatment
                 selectInput(inputId  = "select_into_D", 
                             label    = "Strength Selection into Treatment:",
                             choices  = c("Strong (2)"  = "2",
                                          "Default (1)"  = "1",
                                          "Reduced (0.5)" = "0.5",
                                          "Weak (0.2)"   = "0.2",
                                          "Random (0)"   = "0"), 
                             selected = "1")),

conditionalPanel(condition = "input.comparison_mode != 'analysis' | (input.comparison_mode == 'analysis' & 
                 input.analysisDim != 'Ynoise')", 
                 
                 # Select whether additional noise is added to Y1/Y0
                 selectInput(inputId  = "Ynoise", 
                             label    = "Noise in Outcome Specification:",
                             choices  = choices_Ynoise, 
                             selected = "1"))

),


conditionalPanel(condition = "input.datatype == 'real_nsw'", ##############################################

# Select gender
selectInput(inputId = "gender",
            label = "Gender:",
            choices = c("Men" = "men",
                        "Women" = "women"),
            selected = "men"),

# Select subsample
selectInput(inputId = "subsample",
          label = "Subsample:",
          choices = c("Lalonde" = "Lalonde",
                      "Dehejia/Wahba" = "Dehejia",
                      "Smith/Todd" = "SmithTodd"),
          selected = "Dehejia"),

# Select experimental group
selectInput(inputId = "exper_group",
            label = "Experimental group:",
            choices = c("Treatment" = "treat",
                        "Control" = "control"),
            selected = "control"),

conditionalPanel(condition = "input.gender == 'women'", ######################
             # Select comparison group
             selectInput(inputId = "comp_group_women",
                         label   = "Comparison group:",
                         choices = c("NSW"   = "nsw",
                                     "PSID1" = "psid1",
                                     "PSID2" = "psid2"),
                         selected = "psid1")
                 ),

conditionalPanel(condition = "input.gender == 'men'", ######################

            # Select comparison group
            selectInput(inputId = "comp_group_men",
                        label   = "Comparison group:",
                        choices = c("NSW"   = "nsw",
                                    "PSID1" = "psid1",
                                    "PSID2" = "psid2",
                                    "PSID3" = "psid3",
                                    "CPS1"  = "cps1",
                                    "CPS2"  = "cps2",
                                    "CPS3"  = "cps3"),
                        selected = "psid1")
                 )

)

)
 