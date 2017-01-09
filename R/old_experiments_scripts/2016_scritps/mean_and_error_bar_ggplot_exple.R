performence_stats_data = data.frame("Models"=c("Model1","Model1","Model1","Model2","Model2","Model2","Model3","Model3","Model3"), 
                                    "Stats"=c('Rsquared','RSR', 'PB/100','Rsquared','RSR','PB/100','Rsquared','RSR','PB/100'),
                                    "Values"=c(0.76, 0.48, 0.124, 0.85, 0.37, 0.054, 0.87, 0.35, 0.062) )

copy_performence_stats_data = performence_stats_data # copy the data frame just in case i need it later
# adding IQR column
performence_stats_data$iqr<-c(0.24, 0.23, 0.191, 0.13, 0.17, 0.153, 0.12, 0.15, 0.159)

limits <- aes(ymax = performence_stats_data $Values + performence_stats_data $iqr,
              ymin = performence_stats_data $Values - performence_stats_data $iqr)


p <- ggplot(data = performence_stats_data, aes(x=Stats, y=Values, fill=Models)) + 
    geom_bar(stat="identity",position="dodge") + xlab("Performence Statistics")

p <- ggplot(data = perform_stats_to_plot, aes(x=variable_label, y=value, fill=model)) + 
    geom_boxplot() + xlab("Performence Statistics")

