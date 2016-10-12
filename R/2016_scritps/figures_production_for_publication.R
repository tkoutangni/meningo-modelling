# now need to print all plots to a single pdf
base_text = "for 64 health center-years with complete data, across four health distrits of \nBurkina faso (2004-2010)."

pdf(file = "../figs/final_figs/hyperendemic_trajectory_matching_plots.pdf")


layout(1) # set graphical area for text display
add_text_to_plot("Appendix 3.",font_cex = 1, valign = "center", halign = "center")

# ploting raw data time series.
#layout(1) # set graphical area for text display
#model_1_fig_text = bquote("Figure 1. Time series of available hyperendemic meningitis data across health centers of four \nhealth districts in Burkina-Faso (2004-2012).")
#add_text_to_plot( model_1_fig_text, font_cex=1, valign = 'center', halign = 'left')

#ggplot_time_series_all_years(rbindlist(melt_non_epi_data_list),ncol=4)

ggplot_time_series_all_years(hounde_melt_data_all_year, ncol = 3)
ggplot_time_series_all_years(other_district_melt_data_all_year, ncol = 3)

# plot trajectory matching plots for all health center-years
layout(1) # set graphical area for text display
model_1_fig_text = paste('Figure 1. Model1-"inv" trajectories matching plots of simulated and observed (weekly) \ndata, ',base_text)
add_text_to_plot( model_1_fig_text, font_cex=1, valign = 'center', halign = 'left')
graphSettings() # set the graphical parameters.

# Model 1 plotings
is_a0Constant = FALSE;
model_1_performence = computeModelPredictionAndPerformStat(a0ForcEstimates_matrice)

# Model 2 plottings
layout(1) # set graphical area for text display
model_1_fig_text = paste('Figure 2. Model2-"transm" trajectories matching plots of simulated and observed (weekly) \ndata, ',base_text)
add_text_to_plot( model_1_fig_text, font_cex=1, valign = 'center', halign = 'left')
graphSettings() # set the graphical parameters.

is_a0Constant = TRUE;
model_2_performence = computeModelPredictionAndPerformStat(beta0ForcEstimates_matrice)

# Model 3 plottings
layout(1) # set graphical area for text display
model_1_fig_text = paste('Figure 3. Model3-"inv-transm" trajectories matching plots of simulated and observed \n(weekly) data, ',base_text)
add_text_to_plot( model_1_fig_text, font_cex=1, valign = 'center', halign = 'left')
graphSettings() # set the graphical parameters.

is_a0Constant = FALSE;
model_3_performence = computeModelPredictionAndPerformStat(a0_beta0_ForcEstimates_matrice)

dev.off()
