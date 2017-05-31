# load the new computation of some hc mle_estimates after correcting the fitting function returning NAN in those hc centers.
rm(list=ls())
load('R/some_hc_mle_estimates_no_age_str.RData'); load('R/some_hc_mle_estimates_with_age_str.RData') # reestimation result from pascal
load("R/some_hc_mle_estimates_no_age_str_model_2.RData") # Partial reestimation results for model 2 from me.

# load estimates for simulations with no age str
load("data/processed_data/new_processed_february_2016/mle_a0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_beta0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_a0_beta0_ForcEstimates_matrice.RData")

# load estimates for simulations with no age str
load("data/processed_data/new_processed_february_2016/mle_age_str_a0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_age_str_beta0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_age_str_a0_beta0_ForcEstimates_matrice.RData")

# Names of those health centers and their column numbers in observed data
mle_a0ForcEstimates_matrice[which(mle_a0ForcEstimates_matrice$f_val>100), c("district", "hc", "year","f_val")]
# model2
mle_beta0ForcEstimates_matrice[which(mle_beta0ForcEstimates_matrice$f_val>100), c("district", "hc", "year","f_val")]
#model3
mle_a0_beta0_ForcEstimates_matrice[which(mle_a0_beta0_ForcEstimates_matrice$f_val>100), c("district", "hc", "year","f_val")]

# age structured model estimations
mle_age_str_a0ForcEstimates_matrice[which(mle_age_str_a0ForcEstimates_matrice$f_val>100), c("district", "hc", "year","f_val")]
#Model2
mle_age_str_beta0ForcEstimates_matrice[which(mle_age_str_beta0ForcEstimates_matrice$f_val>100), c("district", "hc", "year","f_val")]

# check to see if all health center years are present in all health center.
#Model3
mle_age_str_a0_beta0_ForcEstimates_matrice[which(mle_age_str_a0_beta0_ForcEstimates_matrice$f_val>100), c("district", "hc", "year","f_val")]

# define some usefull functions 
# 

# replace_colums_values<-function(estimates_matrice, row_indexes,
#                                 new_values_matrice){
#         n_col = dim(estimates_matrice)[2]
#         if(n_col == dim(new_values_matrice)[2]){
#                 estimates_matrice[row_indexes, ]<-new_values_matrice
#                 # print the part of the matrice that was changed for verifications
#                 #return (estimates_matrice)
#                 print(estimates_matrice[row_indexes,])
#         }
#         else{
#                 stop("COSTUM MESSAGE: estimates_matrice is not of the same dimension as new_values_matrice\n")
#         }
# }

get_index_of_estimates_to_change <-
        function(estimates_matrice, year, district, hc_vector) {
                which(
                        estimates_matrice[,"year"] == year &
                                startsWith(rownames(estimates_matrice),district) &
                                estimates_matrice[,"hc"] %in% c(hc_vector)
                )
        }

#load('R/some_hc_mle_estimates_with_age_str.RData'); names_mle_with_age_str = c(ls())
#################################
# Model With no age structure
#################################

# find rows of model 2 estimates matrice to replace
indexes_seguen_2007_model2 = get_index_of_estimates_to_change(mle_beta0ForcEstimates_matrice, year = 2007, "SEGUENEGA", c(1,3,6))

indexes_seguen_2010_model2 = get_index_of_estimates_to_change(mle_beta0ForcEstimates_matrice, year = 2010, "SEGUENEGA", c(2,5))

indexes_hounde_2006_model2 = get_index_of_estimates_to_change(mle_beta0ForcEstimates_matrice, year = 2006, "HOUNDÉ", c(10))

indexes_hounde_2008_model2 = get_index_of_estimates_to_change(mle_beta0ForcEstimates_matrice, year = 2008, "HOUNDÉ", c(2))

indexes_kvigue_2010_model2 = get_index_of_estimates_to_change(mle_beta0ForcEstimates_matrice, year = 2010, "KARANGASSO VIGUE", c(3))

mle_beta0ForcEstimates_matrice[indexes_seguen_2007_model2,]<- mle_non_epi_seguenNonEpiForc_beta0_2007
mle_beta0ForcEstimates_matrice[indexes_seguen_2010_model2,]<- mle_non_epi_seguenNonEpiForc_beta0_2010
mle_beta0ForcEstimates_matrice[indexes_hounde_2006_model2,]<- mle_non_epi_houndeForc_beta0_2006
mle_beta0ForcEstimates_matrice[indexes_hounde_2008_model2,]<- mle_non_epi_houndeForc_beta0_2008
mle_beta0ForcEstimates_matrice[indexes_kvigue_2010_model2,]<- mle_non_epi_kvigueForc_beta0_2010


#find rows of model 3 estimates matrice to replace
indexes_hounde_2006 = get_index_of_estimates_to_change(mle_a0_beta0_ForcEstimates_matrice, year = 2006, "HOUNDÉ", c(14))

indexes_hounde_2008 = get_index_of_estimates_to_change(mle_a0_beta0_ForcEstimates_matrice, year = 2008, "HOUNDÉ", c(2))

indexes_hounde_2009 = get_index_of_estimates_to_change(mle_a0_beta0_ForcEstimates_matrice, year = 2009, "HOUNDÉ", c(15,17))

indexes_kvigue_2010 = get_index_of_estimates_to_change(mle_a0_beta0_ForcEstimates_matrice, year = 2010, "KARANGASSO VIGUE", c(3))

mle_a0_beta0_ForcEstimates_matrice[indexes_hounde_2006,]<- mle_non_epi_houndeForc_a0_beta0_2006
mle_a0_beta0_ForcEstimates_matrice[indexes_hounde_2008,]<- mle_non_epi_houndeForc_a0_beta0_2008
mle_a0_beta0_ForcEstimates_matrice[indexes_hounde_2009,]<- mle_non_epi_houndeForc_a0_beta0_2009
mle_a0_beta0_ForcEstimates_matrice[indexes_kvigue_2010,]<- mle_non_epi_kvigueForc_a0_beta0_2010


#################################
# Model With age structure
#################################
# find rows of model 2 estimates matrice to replace
indexes_seguen_2006_model2_age = get_index_of_estimates_to_change(mle_age_str_beta0ForcEstimates_matrice, year = 2006, "SEGUENEGA", c(3,6))

indexes_seguen_2009_model2_age = get_index_of_estimates_to_change(mle_age_str_beta0ForcEstimates_matrice, year = 2009, "SEGUENEGA", c(6))
indexes_seguen_2010_model2_age = get_index_of_estimates_to_change(mle_age_str_beta0ForcEstimates_matrice, year = 2010, "SEGUENEGA", c(2,3,5))

indexes_hounde_2006_model2_age = get_index_of_estimates_to_change(mle_age_str_beta0ForcEstimates_matrice, year = 2006, "HOUNDÉ", c(4,5,7,10))

indexes_hounde_2008_model2_age = get_index_of_estimates_to_change(mle_age_str_beta0ForcEstimates_matrice, year = 2008, "HOUNDÉ", c(8,18))


# replace the values where appropriate in the age str model 2 estimates matrice  mle_age_str_beta
mle_age_str_beta0ForcEstimates_matrice[indexes_seguen_2006_model2_age,]<- mle_age_str_non_epi_seguenNonEpiForc_beta0_2006
mle_age_str_beta0ForcEstimates_matrice[indexes_seguen_2009_model2_age,]<- mle_age_str_non_epi_seguenNonEpiForc_beta0_2009
mle_age_str_beta0ForcEstimates_matrice[indexes_seguen_2010_model2_age,]<- mle_age_str_non_epi_seguenNonEpiForc_beta0_2010

mle_age_str_beta0ForcEstimates_matrice[indexes_hounde_2006_model2_age,]<- mle_age_str_non_epi_houndeForc_beta0_2006
mle_age_str_beta0ForcEstimates_matrice[indexes_hounde_2008_model2_age,]<- mle_age_str_non_epi_houndeForc_beta0_2008

# model 3 with age structure.
# find rows of model 3 estimates matrice to replace

indexes_seguen_2006_model3_age = get_index_of_estimates_to_change(mle_age_str_a0_beta0_ForcEstimates_matrice, year = 2006, "SEGUENEGA", c(2))

indexes_hounde_2004_model3_age = get_index_of_estimates_to_change(mle_age_str_a0_beta0_ForcEstimates_matrice, year = 2004, "HOUNDÉ", c(13))

indexes_hounde_2006_model3_age = get_index_of_estimates_to_change(mle_age_str_a0_beta0_ForcEstimates_matrice, year = 2006, "HOUNDÉ", c(4))

indexes_lena_2006_model3_age = get_index_of_estimates_to_change(mle_age_str_a0_beta0_ForcEstimates_matrice, year = 2006, "LENA", c(1))

# replace the value in the age str model 3 estimates matrice
mle_age_str_a0_beta0_ForcEstimates_matrice[indexes_seguen_2006_model3_age,]<- mle_age_str_non_epi_seguenNonEpiForc_a0_beta0_2006

mle_age_str_a0_beta0_ForcEstimates_matrice[indexes_hounde_2004_model3_age,]<- mle_age_str_non_epi_houndeForc_a0_beta0_2004

mle_age_str_a0_beta0_ForcEstimates_matrice[indexes_hounde_2006_model3_age,]<- mle_age_str_non_epi_houndeForc_a0_beta0_2006

mle_age_str_a0_beta0_ForcEstimates_matrice[indexes_lena_2006_model3_age,]<- mle_age_str_non_epi_lenaForc_a0_beta0_2006

# save the modification 
#save("data/processed_data/new_processed_february_2016/mle_a0ForcEstimates_matrice.RData")
save(mle_beta0ForcEstimates_matrice, file= "data/processed_data/new_processed_february_2016/mle_beta0ForcEstimates_matrice.RData")
save(mle_a0_beta0_ForcEstimates_matrice, file= "data/processed_data/new_processed_february_2016/mle_a0_beta0_ForcEstimates_matrice.RData")

#save(mle_age_str_a0ForcEstimates_matrice, file = "data/processed_data/new_processed_february_2016/mle_age_str_a0ForcEstimates_matrice.RData")
save(mle_age_str_beta0ForcEstimates_matrice, file =  "data/processed_data/new_processed_february_2016/mle_age_str_beta0ForcEstimates_matrice.RData")
save(mle_age_str_a0_beta0_ForcEstimates_matrice, file = "data/processed_data/new_processed_february_2016/mle_age_str_a0_beta0_ForcEstimates_matrice.RData")

