rm(list=ls())
insert_age_structure = FALSE; heterogenous_mixing=FALSE
source("R/run_first.R")
source("R/models_parameters.R")
dev.off() # clear all graphical devices and plots in the plotting area.
graphSettings() # seeting plotting parameters
#=============================================================
## Parameters estimates when only invasion rate is forced
#=============================================================

# remoove first and last row of data which are NAs
non_missing_data<-function(data.frame){
  # replace NA at the begining and end of the data time series by 0
  #data.frame<-data.frame[c(1,nrow(data.frame)),]
  # data.frame[c(1),]<-0
  # data.frame[c(nrow(data.frame)),]<-0
  non_missing_data=data.frame[complete.cases(data.frame)]
  return(non_missing_data)
  }









############################################################################
## concatenating all the estimates per model type into data frames or matrix 
############################################################################       

# Model with forcing of the invasion alone.

mle_a0ForcEstimates_matrice = rbind(
  mle_non_epi_seguenNonEpiForc_a0_2006,
  mle_non_epi_seguenNonEpiForc_a0_2007,
  mle_non_epi_seguenNonEpiForc_a0_2009,
  mle_non_epi_seguenNonEpiForc_a0_2010,
  mle_non_epi_houndeForc_a0_2004,
  mle_non_epi_houndeForc_a0_2006,
  mle_non_epi_houndeForc_a0_2007,
  mle_non_epi_houndeForc_a0_2008,
  mle_non_epi_houndeForc_a0_2009,
  mle_non_epi_lenaForc_a0_2006,
  mle_non_epi_lenaForc_a0_2007,
  mle_non_epi_kvigueForc_a0_2008,
  mle_non_epi_kvigueForc_a0_2010)

save(mle_a0ForcEstimates_matrice,
     file = "data/processed_data/new_processed_february_2016/mle_a0ForcEstimates_matrice.RData")


# Model with forcing of the transmission alone. 

mle_beta0ForcEstimates_matrice = rbind(
  mle_non_epi_seguenNonEpiForc_beta0_2006,
  mle_non_epi_seguenNonEpiForc_beta0_2007,
  mle_non_epi_seguenNonEpiForc_beta0_2009,
  mle_non_epi_seguenNonEpiForc_beta0_2010,
  mle_non_epi_houndeForc_beta0_2004,
  mle_non_epi_houndeForc_beta0_2006,
  mle_non_epi_houndeForc_beta0_2007,
  mle_non_epi_houndeForc_beta0_2008,
  mle_non_epi_houndeForc_beta0_2009,
  mle_non_epi_lenaForc_beta0_2006,
  mle_non_epi_lenaForc_beta0_2007,
  mle_non_epi_kvigueForc_beta0_2008,
  mle_non_epi_kvigueForc_beta0_2010
  
)

save(mle_beta0ForcEstimates_matrice,
     file = "data/processed_data/new_processed_february_2016/mle_beta0ForcEstimates_matrice.RData")


# Model with forcing of both the transmission and invasion rate. 

mle_a0_beta0_ForcEstimates_matrice = rbind(
  mle_non_epi_seguenNonEpiForc_a0_beta0_2006,
  mle_non_epi_seguenNonEpiForc_a0_beta0_2007,
  mle_non_epi_seguenNonEpiForc_a0_beta0_2009,
  mle_non_epi_seguenNonEpiForc_a0_beta0_2010,
  mle_non_epi_houndeForc_a0_beta0_2004,
  mle_non_epi_houndeForc_a0_beta0_2006,
  mle_non_epi_houndeForc_a0_beta0_2007,
  mle_non_epi_houndeForc_a0_beta0_2008,
  mle_non_epi_houndeForc_a0_beta0_2009,
  mle_non_epi_lenaForc_a0_beta0_2006,
  mle_non_epi_lenaForc_a0_beta0_2007,
  mle_non_epi_kvigueForc_a0_beta0_2008,
  mle_non_epi_kvigueForc_a0_beta0_2010)

save(mle_a0_beta0_ForcEstimates_matrice,
     file = "data/processed_data/new_processed_february_2016/mle_a0_beta0_ForcEstimates_matrice.RData")
