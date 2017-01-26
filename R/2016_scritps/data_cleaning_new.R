# clean and  prepare data for analysis.
load("data/raw_data/data_fev_2015/141118_BDD_annuellehebdomadaire_mergee.RData")

# get names of the database columns.
names(ah)

# subset colums i'm interested in.
raw_data_subset <-
    subset(
        ah,
        select = c(
            "region.x",
            "region.y",
            "district",
            "fs",
            "annee",
            "semaine",
            "meningite.cas",
            "population",
            "distance"
        )
    )

# sort data.frame by variables region, district, annee, and semaine
raw_data_subset_sort<- raw_data_subset[with(raw_data_subset, order(district, annee, semaine)), ]

# view colnames of the sorted data.frame
colnames(raw_data_subset_sort)

# view actual first and last five line
head(raw_data_subset_sort, 5)
tail(raw_data_subset_sort, 5)

# get number of line and columns of the sorted database
cat("\n Data base has: \n - ", dim(raw_data_subset_sort)[1], "observations.\n", "- ", dim(raw_data_subset_sort)[2], "columns." )

# Districts in the data.frame
districts.names<- unique(raw_data_subset_sort$district)

cat("\n The data.frame contains ", as.numeric(length(districts.names)), "districts which are:\n", paste0(districts.names,  collapse = ", "))

# The districts are located in regions
regions.names<- unique(raw_data_subset_sort$region.x)
cat("\n The ",as.numeric(length(districts.names)), " districts are located in ", as.numeric(length(regions.names)), "regions of Burkina Faso which are:\n", paste0(regions.names,  collapse = ", "))

# Health centers in each district.

health_centers_per_district <-list_district_health_centers(raw_data_subset_sort)

# number of health centers per district.
health_centers_count = sapply(health_centers_per_district, 
       function(x){
           length(x)
           #paste0(x, collapse = ', ')
       }
    )

cat("\n The number of health centers per district are as follows:\n", paste0(names(health_centers_count), ":", health_centers_count, sep="", collapse = ", "))

# Health centers names per district.
district_health_centers_names = sapply(health_centers_per_district,
                                       function(x) {
                                           paste0(x, collapse = ', ')
                                       })

cat("\n The names of health centers per district are as follows:\n", paste0(names(district_health_centers_names), ": ", district_health_centers_names, sep="\n", collapse = " ", "\n"))


# an alternative way to get the health centers names in each district
alt_district_health_centers_names = by(raw_data_subset_sort, raw_data_subset_sort[, "district"], function(x){unique(x$fs)})

# show content of the variable
alt_district_health_centers_names

# subset the data by district
data_subset_by_district = by(raw_data_subset_sort, raw_data_subset_sort[,"district"], function(x){return(x)})
# Look at the 5 first observations of each district data.
lapply(data_subset_by_district, head, 4)

# Now split the each district data.frame into it health centers data

data_subset_by_districts_and_health_centers = lapply(data_subset_by_district, function(x){
    health_centers = x$fs # important to store the grouping variable
    split(x, health_centers)
})

# add date column to the data_subset_by_districts_and_health_centers
data_subset_by_districts_and_health_centers = 
    lapply(data_subset_by_districts_and_health_centers, function(x){
    lapply(x, function(y){
        y$date = dmy("28/12/2003") + weeks(0:dim(y)[1])[-1]
        y$meningite.cas.incidence<-y$meningite.cas/y$population
        return(y) # very important to return y otherwise
        # the column `date` is not added to the data.frames
    })
})


# split data per district that are included in the initial analysis.
# Population size per health center year in all district.
# 1st define function to get the population per health center year
    get_health_center_year_population = function(data_list_of_list) {
        lapply(data_list_of_list, function(x) {
            lapply(x, function(y) {
                by(y, y[, c("annee", "fs")], function(splited_data) {
                    splited_data_population = splited_data$population
                })
            })
        })
    }

# now get the health center years data using the get_health_center_year_population function

all_health_center_year_population = get_health_center_year_population(data_subset_by_districts_and_health_centers)
    
# store population size of the health centers years used in initial analysis in a list.
# all_health_center_year_population$SEGUENEGA$`csps bema`["2006",]

get_population_size<-function(data = all_health_center_year_population, district.name, health_center_name, year){
    #district.name = as.symbol(district.name); year = as.symbol(health_center_name)
    population_size = unlist(data[[district.name]][[health_center_name]][year,])
    return(unique(population_size))
}



seguen_2006_population_size = list(
    "csps bema" = get_population_size(district.name = "SEGUENEGA", health_center_name = "csps bema", year = "2006"),
    "csps berenga" = get_population_size(district.name = "SEGUENEGA", health_center_name = "csps berenga", year = "2006"), 
    "csps bouga" = get_population_size(district.name = "SEGUENEGA", health_center_name = "csps bouga", year = "2006"), 
    "csps gambo" = get_population_size(district.name = "SEGUENEGA", health_center_name = "csps gambo", year = "2006"),
    "csps goubré" = get_population_size(district.name = "SEGUENEGA", health_center_name = "csps goubré", year = "2006"),
    "csps kossouka" = get_population_size(district.name = "SEGUENEGA", health_center_name = "csps kossouka", year = "2006"),
    "csps rondo" = get_population_size(district.name = "SEGUENEGA", health_center_name = "csps rondo", year = "2006"),
    "csps teonsgo" = NA
)



non_epi_health_center_year_population_size<-list(
    seguen_2006_population_size = seguen_2006_population_size
)


#names(non_epi_health_center_year_data)<-c(
    #"seguen_2006" , "seguen_2007", "seguen_2008", "seguen_2009", "seguen_2010",
    #"hounde_2004", "hounde_2005", "hounde_2006", "hounde_2007", "hounde_2008", 
    #"hounde_2009", "hounde_2010", "lena_2004", "lena_2005", "lena_2006", "lena_2007",
    #"lena_2008", "lena_2010", "lena_2010", "Kvigue_2008", "Kvigue_2009", "Kvigue_2010"
#)






test = data_subset_by_districts_and_health_centers$SEGUENEGA$`csps bema`
# melt test data subset
test_melt = melt(subset(test, select=c("date", "meningite.cas.incidence")), id.vars = "date", variable.name = "variable", value.name="value")
#plot the data.

p<-ggplot_line_graph(test_melt, "date", "value", "variable")
multi_ggplot(p,p, cols = 2)

# plot 2004 data in the bema health center
graphSettings()
by(test, test[, "annee"], function(x){
   plot(x$semaine,x$meningite.cas/x$population,  type="b", pch=16, xlab = "", ylab = "")
    title(main = paste0(c(unique(x$annee),unique(x$fs)), collapse = ", "), xlab = "Calendar weeks", ylab="Cases")
    #return(c(unique(x$annee), unique(x$fs)))
    })

# visualize all data now.
# melt data in each health center and prepare for ggplot
data_subset_by_districts_and_health_centers_melt = lapply(data_subset_by_districts_and_health_centers, function(x){
    lapply(x, function(y){
        health_center_melt_data = 
        melt(subset(y, select=c("date","meningite.cas.incidence")), id.vars = "date", variable.name = "variable", value.name="value")
        return(health_center_melt_data)
    })
})

#visualize health centers time series data per district.

test = function(data_subset_melt){
    toto = lapply(data_subset_melt, 
       function(x){
           p<-lapply(x, function(z){
               p<-ggplot_line_graph(z,"date","value", "variable")
               p + labs(list(title ="title here" ,"Title", x = "Time", y = "Incidence"))
               
           })
          
       })
    return(toto)
}

test_plot = test(data_subset_melt = data_subset_by_districts_and_health_centers_melt)



