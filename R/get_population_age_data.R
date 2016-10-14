
get_population_age_stat_data <- function(country, year) {
    # @country = should be FIPS_country_codes as listed at:
    # https://en.wikipedia.org/wiki/List_of_FIPS_country_codes
    # eg : "UV" for UPPER VOLTA (burkina faso)
    # base url for 5 year class
    # base_url<- "http://www.census.gov/population/international/data/idb/region.php?N=%20Results%20&T=10&A=separate&RT=0&Y=" 
    base_url = "http://www.census.gov/population/international/data/idb/region.php?N=%20Results%20&T=15&A=separate&RT=0&Y="
    country_query <- "&R=-1&C="
    url <- paste0(base_url, year, country_query, country)
    df <- data.frame(readHTMLTable(url))
    keep <- c(2, 3, 4, 5, 6) #this specify the column of the data table to keep
    df <- df[,keep]  
    names(df) <- c("Age", "Both sex", "Male", "Female", "Both sex percent")
    cols <- c(2:4)
    df[,cols] <- apply(df[,cols], 2, function(x) as.numeric(as.character(gsub(",", "", x))))
    df <- df[df$Age != 'Total', ]
    df <- df[df$Age != 'Median Age', ]
    # Activate line bellow if the goal is to plot the age pyramid
    # df$Male <- -1 * df$Male 
    df$Age <- factor(df$Age, levels = df$Age, labels = df$Age)
    
    #df.melt <- melt(df, 
                    #value.name='Population', 
                    #variable.name = 'Gender', 
                    #id.vars='Age' )
    
    #return(df.melt)
    return(df)
}  # end get_population_age_stat_data function


gg_prop <- ggplot(data = data.frame()
                  , aes(x = `Age group`, y = Population)) + 
    geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3,
             fill="white", colour="black", width = .5) +  
    scale_y_continuous(labels = percent) +    
    scale_fill_few('medium', drop = FALSE) +  theme_bw() +            # keep levels, if data is filtered
    labs(x = 'Age groups', y = NULL, fill = 'Legend'
         , title = 'Age distribution of Burkina Faso')  # what's the reader looking at?

####################################
# getting and ploting age distribution data.
 
####################################
burkina_age_data = get_population_age_stat_data(country = "UV", 2006)
burkina_age_data$`Age class`<-NA
burkina_age_data[c("0":"4")+1,]$`Age class`<-"[0-4]"
burkina_age_data[c("5":"12")+1,]$`Age class`<-"[5-12]"
burkina_age_data[c("13":"19")+1,]$`Age class`<-"[13-19]"
burkina_age_data[c(21:89),]$`Age class`<-"20+"

# aggragate the data into specific age classes
# Compute the sum for the variables in 'burkina_age_data', grouped
# according to the age classe variable.

data_by_age_group = aggregate(burkina_age_data[,c(2:4)], by = list("Age group" = burkina_age_data$`Age class`), sum)

data_by_age_group$`Both sex proportion`<- (data_by_age_group$`Both sex`)/sum(data_by_age_group$`Both sex`)

data_by_age_group$`Male proportion`<- (data_by_age_group$Male)/sum(data_by_age_group$`Both sex`)

data_by_age_group$`Female proportion`<- (data_by_age_group$Female)/sum(data_by_age_group$`Both sex`)

burkina_age_data_melt = melt(data_by_age_group, value.name = "Population", id.vars = "Age group")



#p1<-gg_prop %+% subset(burkina_age_data_melt, variable=="Both sex proportion")

#multi_ggplot(p1,p1, cols = 2)

