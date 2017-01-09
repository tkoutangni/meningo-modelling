# code chunk for quick reference

#=========================================================================
##Remove all variable in R environement starting with a given character.
rm(list=ls(pattern="^toto"))
#====================================================================
# An exemple of how to get some variable from the global env
# and put them in a list using a vector of names of the elements
# that will constitute the named list.
# 

hyperendemic_data_frame_name = c(ls()[c(grep("_200", ls()))])

hyperendemic_data_list = setNames(
    lapply(hyperendemic_data_frame_name, function(x)
        assign(x, eval(as.symbol(x)))),
    c(hyperendemic_data_frame_name)
)


lapply(hyperendemic_data_list, class)


# copy the hyperendemic_data_list

copy_hyperendemic_data_list = hyperendemic_data_list
#remove the julian_day colum for ploting
copy_hyperendemic_data_list = lapply(copy_hyperendemic_data_list, remove_unwanted_column, c('julian_day'))

# Prepare data to be ploted by ggplot2
# Basically melt the time series into a data-frame that can be processed by ggplot
hyperendemic_data_melt = lapply(copy_hyperendemic_data_list, melt_data_all_year)

#head(hyperendemic_data_melt$hounde_2005,5)

lapply(hyperendemic_data_melt, ggplot_rbindlist_data_frames)

# remove some of the repeated data.
hyperendemic_data_melt$ouahigouya_2004  <-NULL
hyperendemic_data_melt$seguen_2004_2010 <-NULL
hyperendemic_data_melt$seguen_2006_2007 <-NULL
hyperendemic_data_melt$seguen_2006_2010 <-NULL

# List the health centers included in the hyperendemic
# modelling.
# 
levels(test$health_center_name)

# Plot usable hyperendemic data from the database.

ggplot_time_series_all_years(rbindlist(hyperendemic_data_melt))


### sort data frame by column(s)

dd <- data.frame(b = factor(c("Hi", "Med", "Hi", "Low"), 
                            levels = c("Low", "Med", "Hi"), ordered = TRUE),
                 x = c("A", "D", "A", "C"), y = c(8, 3, 9, 9),
                 z = c(1, 1, 1, 2))

dd[with(dd, order(-z, b)), ]

