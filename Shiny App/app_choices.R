#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This file stores some app choice options (to retrieve name for graphs)
#
#########################################################################################################

choices_YX <- c("YA: Additivity and linearity (main effects only)" = "YA",
                 #"YB: Mild non-linearity (one quadratic term)" = "YB",
                 "YC: Moderate non-linearity (three quadratic term)" = "YC",
                 #"YD: Mild non-additivity (four two-way interaction term)" = "YD",
                 "YE: Mild non-additivity and non-linearity \n (three two-way interaction terms and one quadratic term)"  = "YE",
                 #"YI: Mild non-additivity and moderate non-linearity \n (four two-way interaction terms and three quadratic terms)" = "YI",
                 "YF: Moderate non-additivity (ten two-way interaction terms)" = "YF",
                 #"YH: Moderate non-additivity and mild non-linearity \n (ten two-way interaction terms and one quadratic term)" = "YH",
                 "YG: Moderate non-additivity and non-linearity \n (ten two-way interaction terms and three quadratic terms)" = "YG" )

choices_YX_short <- c("YA: add. & lin." = "YA",
                #"YB: Mild non-linearity" = "YB",
                "YC: Non-lin." = "YC",
                #"YD: Mild non-additivity" = "YD",
                "YE: Mild non-add. \n & non-lin."  = "YE",
                #"YI: Mild non-additivity and moderate non-linearity" = "YI",
                "YF: Non-add." = "YF",
                #"YH: Moderate non-additivity and mild non-linearity" = "YH",
                "YG: Mod. non-add. \n & non-lin." = "YG" )


choices_DX <- c("DA: Additivity and linearity (main effects only)" = "DA",
                 #"DB: Mild non-linearity (one quadratic term)" = "DB",
                 "DC: Moderate non-linearity (three quadratic term)" = "DC",
                 #"DD: Mild non-additivity (four two-way interaction term)" = "DD",
                 "DE: Mild non-additivity and non-linearity \n (three two-way interaction terms and one quadratic term)" = "DE",
                 #"DI: Mild non-additivity and moderate non-linearity \n (four two-way interaction terms and three quadratic terms)" = "DI",
                 "DF: Moderate non-additivity (ten two-way interaction terms)" = "DF",
                 #"DH: Moderate non-additivity and mild non-linearity \n (ten two-way interaction terms and one quadratic term)" = "DH",
                 "DG: Moderate non-additivity and non-linearity \n (ten two-way interaction terms and three quadratic terms)" = "DG")

choices_DX_short <- c("DA: add. & lin." = "DA",
                #"DB: Mild non-linearity" = "DB",
                "DC: Non-lin." = "DC",
                #"DD: Mild non-additivity" = "DD",
                "DE: Mild non-add. \n & non-lin." = "DE",
                #"DI: Mild non-additivity and moderate non-linearity" = "DI",
                "DF: Non-add." = "DF",
                #"DH: Moderate non-additivity and mild non-linearity" = "DH",
                "DG: Mod. non-add. \n & non-lin." = "DG")


misspec_choices <- c("Both PS and Cond. Outcome correctly specified"      = "bothCorrect",
                     "PS correctly specified, Cond. Outcome misspecified" = "PSCorrect",
                     "PS misspecified, Cond. Outcome correctly specified" = "COCorrect",
                     "Both PS and Cond. Outcome misspecified"             = "bothMisspecified")

choices_Ynoise <- c("10"  = "10",
                    "5"  = "5",
                    "2"  = "2",
                    "1"  = "1",
                    "0.5"  = "0.5",
                    "0.2"  = "0.2",
                    "0"   = "0")


choices_Dclassification_PS <- c("Yes" = "1",
                                "No"  = "0")
