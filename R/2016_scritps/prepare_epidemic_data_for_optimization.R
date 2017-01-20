###============================================================
# preparing epidemic data_frame for use in optimation routine.
#==============================================================
epidata_dafra_2006 = dfEpi_list_2006_with_julian_day$dfEpiDafra
epidata_gourcy_2006 = dfEpi_list_2006_with_julian_day$dfEpiGourcy
epidata_seguenega_2006 = dfEpi_list_2006_with_julian_day$dfEpiSeguenega
epidata_lena_2006 = dfEpi_list_2006_with_julian_day$dfEpiLena
epidata_kvigue_2006 = dfEpi_list_2006_with_julian_day$dfEpiKvigue

epidata_hounde_2006 = dfEpi_list_2006_with_julian_day$dfEpiHounde
epidata_dande_2006 = dfEpi_list_2006_with_julian_day$dfEpiDande
epidata_gourcy_2007 = dfEpi_list_2007_with_julian_day$dfEpiGourcy
epidata_orodara_2007 = dfEpi_list_2007_with_julian_day$dfEpiOrodora
epidata_seguenega_2007 = dfEpi_list_2007_with_julian_day$dfEpiSeguenega
epidata_boulsa_2008 = dfEpi_list_2008_with_julian_day$dfEpiboulsa
epidata_gourcy_2008 = dfEpi_list_2008_with_julian_day$dfEpiGourcy
epidata_orodara_2008 = dfEpi_list_2008_with_julian_day$dfEpiOrodora
epidata_seguenega_2010 = dfEpi_list_2010_with_julian_day$dfEpiSeguenega


#========================================================================
# computing models predictions and performance stats for epidemic data.
#========================================================================
# Some function to compute models-specific simulations

model3_epi_sim = function(zoo_serie, district_id, year_now) {
    is_a0Constant=FALSE; #graphSettings()
    parmEstimates = yearSpecFit(
        district_id = district_id,
        district_year_data = zoo_serie,
        year_now = year_now, hc_vector = c(1:(dim(zoo_serie)[2] - 1)),
        a0ForcingOnly = FALSE, beta0ForcingOnly = FALSE,
        addCarriageConstrain = FALSE, show_plot = TRUE,
        nloptrAlgorithm = "NLOPT_LN_NELDERMEAD",  # NLOPT_LN_BOBYQA  # NLOPT_LN_COBYLA #NLOPT_LN_NELDERMEAD
        n_iter = 5000
    )
    return(parmEstimates)
} # ends model3_epi_sim function

model2_epi_sim = function(zoo_serie, district_id, year_now) {
    is_a0Constant=TRUE; #graphSettings()
    parmEstimates = yearSpecFit(
        district_id = district_id,
        district_year_data = zoo_serie,
        year_now = year_now, hc_vector = c(1:(dim(zoo_serie)[2] - 1)),
        a0ForcingOnly = FALSE, beta0ForcingOnly = TRUE,
        addCarriageConstrain = FALSE, show_plot = TRUE,
        nloptrAlgorithm = "NLOPT_LN_NELDERMEAD",  # NLOPT_LN_BOBYQA  # NLOPT_LN_COBYLA #NLOPT_LN_NELDERMEAD
        n_iter = 5000
    )
    return(parmEstimates)
} # ends model2_epi_sim function



is_a0Constant=FALSE; graphSettings()
test_params_estimates_2006_NELDER_M_M3 = lapply(dfEpi_list_2006_with_julian_day, model3_epi_sim, district_id=6,year_now=2006)
test_params_estimates_2007_NELDER_M_M3 = lapply(dfEpi_list_2007_with_julian_day, model3_epi_sim, district_id=6,year_now=2007)
test_params_estimates_2008_NELDER_M_M3 = lapply(dfEpi_list_2008_with_julian_day, model3_epi_sim, district_id=6,year_now=2008)
test_params_estimates_2010_NELDER_M_M3 = lapply(dfEpi_list_2010_with_julian_day, model3_epi_sim, district_id=6,year_now=2010)

#is_a0Constant=TRUE; graphSettings()
#model2_performance_2006 = lapply(test_params_estimates_2006_NELDER_M_M2,computeModelPredictionAndPerformStat_epi)
is_a0Constant=FALSE; graphSettings()
model3_performance_2006 = lapply(test_params_estimates_2006_NELDER_M_M3, computeModelPredictionAndPerformStat_epi)
model3_performance_2007 = lapply(test_params_estimates_2007_NELDER_M_M3, computeModelPredictionAndPerformStat_epi)
model3_performance_2008 = lapply(test_params_estimates_2008_NELDER_M_M3, computeModelPredictionAndPerformStat_epi)
model3_performance_2010 = lapply(test_params_estimates_2010_NELDER_M_M3, computeModelPredictionAndPerformStat_epi)


## Visualizing parameters estimates distribution and performance
## Stats for epidemics simulations.
get_stat_from_performance_list = function(model_perform_list,stat){
    return(model_perform_list[[stat]])
}

get_stat_all_district = function(model_performance_list_all_district,stat_to_get){
    # @stat_to_get must be a string
    stat = c(
        unlist(lapply(model_performance_list_all_district, get_stat_from_performance_list,stat_to_get),use.names = FALSE)
    )
    return(stat)
}

get_stat_all_hc_years = function(stat_to_get) {
    stat_all_hc_years = c(
        get_stat_all_district(model3_performance_2006,stat_to_get),
        get_stat_all_district(model3_performance_2007,stat_to_get),
        get_stat_all_district(model3_performance_2008,stat_to_get),
        get_stat_all_district(model3_performance_2010,stat_to_get)
    )
    return(stat_all_hc_years)
}

rsquared_epi = get_stat_all_hc_years("Rsquared")
rsr_epi = get_stat_all_hc_years("RSR_stat")
pbias_epi = get_stat_all_hc_years("PBIAS_stat")
#get_stat_all_hc_years("RMSE_stat")
stat1 = data.frame(values = rsquared_epi)
stat1$stat_name<-factor("Rsquared")
stat2 = data.frame(values = pbias_epi)
stat2$stat_name<-factor("Pbias (%)")
stat3 = data.frame(values = rsr_epi)
stat3$stat_name<-factor("RSR")
stat_data_frame = rbind(stat1,stat2,stat3)


## plot the quantitative performances:
w<- ggplot(stat_data_frame,aes(x=stat_name,y=values))+geom_boxplot()+facet_wrap(~ stat_name,scales="free")
w<- w + xlab("") + ylab("Value") + ggtitle("Model 3 Performance")

a_fold_epi = get_stat_all_hc_years("a_fold")
beta_fold_epi = get_stat_all_hc_years("beta_fold")

fold_a = data.frame(values = a_fold_epi)
fold_a$stat_name<-factor("Fold-change invasion")
fold_b= data.frame(values = beta_fold_epi)
fold_b$stat_name<-factor("Fold-change transmision")
fold_data_frame = rbind(fold_a,fold_b)

# ggplot for the fold increase.
z<- ggplot(fold_data_frame,aes(x=stat_name,y=log(values)))+geom_boxplot()
z<- z + xlab("") + ylab("Value (log-scale)") + ggtitle("Fold-change of invasion and transmission rate")

multi_ggplot(w,z,x=NULL)


### ploting parameters estimates

model3_param_estimates_epi = rbind(
    rbindlist(test_params_estimates_2006_NELDER_M_M3),
    rbindlist(test_params_estimates_2007_NELDER_M_M3),
    rbindlist(test_params_estimates_2008_NELDER_M_M3),
    rbindlist(test_params_estimates_2010_NELDER_M_M3)
)
model3_param_estimates_epi$alpha<-(1/model3_param_estimates_epi$alpha)/7
model3_param_estimates_epi$phi<-(1/model3_param_estimates_epi$phi)/year
model3_param_estimates_epi$teta<-(model3_param_estimates_epi$teta)/7



toto = as.data.frame(melt(model3_param_estimates_epi))
    
toto = subset(toto,!variable %in%c("district","hc","year","f_val"))
    

toto$variable_label<-factor(toto$variable,labels=c('Mean transmission /day',
                                                   'Carriage duration (weeks)',
                                                   'Imunity duration (years)',
                                                   'Initial Suseptibles',
                                                   'Initial Carriers',
                                                   'Peak time (week number)',
                                                   'Invasion forc.amp.',
                                                   'Transmission forc.amp.',
                                                   'Mean Invasion /day'))


o<- ggplot(toto,aes(x=variable_label,y=value))+geom_boxplot()+facet_wrap(~variable_label, scales = "free")
o<- o + xlab("") + ylab("Value") + ggtitle("M3 Parameters Estimates with epidemic data")
# outlier.colour to override
o<- o + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
o

nf <- layout(matrix(c(1,1,2,2),nrow=2,ncol=2,byrow = TRUE), widths=c(2,1), heights=c(1,2), TRUE)
layout.show(nf)

multi_ggplot(w,o,z,x=NULL,cols = 2)



## compare the parameters estimates in epidemic context with those in hyper
## endemic context

# load the parameters estimates for hyperendemic incidence simulations:
load("~/Projets/MeningoAfrica/meningoafrica-code/data/processed_data/new_processed_february_2016/a0_beta0_ForcEstimates_matrice.RData")

hyperendemic_param_estimates = a0_beta0_ForcEstimates_matrice[,c(4:12)]
hyperendemic_param_estimates$context<-"hyperendemic"
# convert hyperendemic_param_estimates to a /week or /year scale
hyperendemic_param_estimates$alpha<-(1/hyperendemic_param_estimates$alpha)/7
hyperendemic_param_estimates$phi<-(1/hyperendemic_param_estimates$phi)/year
hyperendemic_param_estimates$teta<-(hyperendemic_param_estimates$teta)/7


epidemic_param_estimates = as.data.frame(model3_param_estimates_epi)
epidemic_param_estimates = epidemic_param_estimates[,c(4:12)]
epidemic_param_estimates$context<-"epidemic"


hyperendemic_param_estimates$variable_label<-factor(hyperendemic_param_estimates$variable,labels=c('Mean transmission /day',
                                                   'Carriage duration (weeks)',
                                                   'Imunity duration (years)',
                                                   'Initial Suseptibles',
                                                   'Initial Carriers',
                                                   'Peak time (week number)',
                                                   'Invasion forc.amp.',
                                                   'Transmission forc.amp.',
                                                   'Mean Invasion /day'))
melt_hyperend_estimates = melt(hyperendemic_param_estimates)
melt_epi_estimates = melt(epidemic_param_estimates)
melt_hyperend_estimates$variable_label<-factor(melt_hyperend_estimates$variable,labels=c('Mean transmission /day',
                                                                                              'Carriage duration (weeks)',
                                                                                              'Imunity duration (years)',
                                                                                              'Initial Suseptibles',
                                                                                              'Initial Carriers',
                                                                                              'Peak time (week number)',
                                                                                              'Invasion forc.amp.',
                                                                                              'Transmission forc.amp.',
                                                                                              'Mean Invasion /day'))

melt_epi_estimates$variable_label<-factor(melt_epi_estimates$variable,labels=c('Mean transmission /day',
                                                                                         'Carriage duration (weeks)',
                                                                                         'Imunity duration (years)',
                                                                                         'Initial Suseptibles',
                                                                                         'Initial Carriers',
                                                                                         'Peak time (week number)',
                                                                                         'Invasion forc.amp.',
                                                                                         'Transmission forc.amp.',
                                                                                         'Mean Invasion /day'))


hyperend_and_epi_estim_melt = rbind(melt_hyperend_estimates,
                                    melt_epi_estimates)

v <- ggplot(data = hyperend_and_epi_estim_melt, aes(x=variable_label, y=value)) + 
    geom_boxplot(aes(fill=context))
# if you want color for points replace group with colour=Label
v <- v + facet_wrap( ~ variable_label, scales="free",nrow = 4)
v <- v + xlab("") + ylab("Value") + ggtitle("")
v <- v + guides(fill=guide_legend(title="Context"))
v # important to run until this line to show the plot.
