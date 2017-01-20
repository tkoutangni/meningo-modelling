# Data visualisation for hyperendemic data.

hyperendemic_data_frame_name = c(ls()[c(grep("_200", ls()))])

hyperendemic_data_list = setNames(
    lapply(hyperendemic_data_frame_name, function(x)
        assign(x, eval(as.symbol(x)))),
    c(hyperendemic_data_frame_name)
)

# copy the hyperendemic_data_list

copy_hyperendemic_data_list = hyperendemic_data_list
#remove the julian_day colum for ploting
copy_hyperendemic_data_list = lapply(copy_hyperendemic_data_list, remove_unwanted_column, c('julian_day'))

# Prepare data to be ploted by ggplot2
# Basically melt the time series into a data-frame that can be processed by ggplot
hyperendemic_data_melt = lapply(copy_hyperendemic_data_list, melt_data_all_year)

#head(hyperendemic_data_melt$hounde_2005,5)

# remove some of the repeated data.
hyperendemic_data_melt$ouahigouya_2004  <-NULL
hyperendemic_data_melt$seguen_2004_2010 <-NULL
hyperendemic_data_melt$seguen_2006_2007 <-NULL
hyperendemic_data_melt$seguen_2006_2010 <-NULL

# List the health centers included in the hyperendemic
# modelling.
# 
levels(hyperendemic_data_melt$health_center_name)

# Plot usable hyperendemic data from the database.

hyperendemic_data_plot<-ggplot_time_series_all_years(rbindlist(hyperendemic_data_melt), ncol = 4)

hyperendemic_data_plot + theme_bw()


# lapply(hyperendemic_data_melt, ggplot_time_series_all_years)

