# file containing experimental function to be moved to myfunction.R
# file in their final useful version.
# 
# A ggplot function to plot age structured model simulation output.
## ggplot options:
library(ggplot2)
theme_opts = theme(
    axis.text = element_text(size = 14),
    legend.key = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white"),
    legend.position = c("top"),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13)
    #panel.grid.major = element_line(colour = "grey40"),
    #panel.grid.minor = element_blank()
)

# a fonction to prepare model output data.frame for use with the ggplot function.
melt_any_data.frame = function(data.frame, x_axis_variable, yaxis_variables) {
    data.frame.subset = subset(data.frame, select = c(x_axis_variable, yaxis_variables))
    melt_out <-
        melt(
            data = data.frame.subset, id = x_axis_variable, variable.name = "Variable", value.name = "Value"
        )
    return(melt_out)
}

# for ploting line graph, especially melt time series data frames
# such as model outputs or incidence curves.
# all arguments expect the data must correspond to a column name in the melt data_frame

ggplot_line_graph <- function(melt_data, x_colname , value_colname  , variable_colname){
    # all arguments expect the data must correspond to a column name in the melt data_frame
    time = as.symbol(x_colname)
    value = as.symbol(value_colname)
    variable = as.symbol(variable_colname)
    p <- qplot(x = eval(time), y = eval(value), data = melt_data, geom = "line", 
               linetype = eval(variable))
    p = p + theme_bw()
    p = p + theme_opts # My custom options as stored in the variable theme_opts
    #p+scale_color_manual(name = variable_colname, values = c(1:4))
    p = p + guides(linetype = guide_legend(title = variable_colname))
    p 
    update_labels(p, labels = list(x = x_colname, y = value_colname))
    
} # end function.

ggplot_costum_x_axis<-function(melt_data, time_step, tick_label_evry){
    axis_ticks = round(seq(0, max(melt_data[,1]), time_step))
    ticks_label = index(axis_ticks)-1
    scale_x_continuous(
        breaks = axis_ticks*tick_label_evry, labels = ticks_label*tick_label_evry) 
}

# costum poportion or percentage barchart with ggplot.
gg_prop <- function(data = data.frame()){ggplot(data = data
                  , aes(x = `Age group`, y = Population)) + 
    geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3,
             fill="white", colour="black", width = .5) +  
    scale_y_continuous(labels = percent) +    
    scale_fill_few('medium', drop = FALSE) +  theme_bw() +            # keep levels, if data is filtered
    labs(x = 'Age groups', y = NULL, fill = 'Legend'
         , title = 'Age distribution of Burkina Faso')
}


