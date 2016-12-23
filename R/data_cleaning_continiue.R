
# Additionnal data cleaning and preparation of data for model fitting 

# remove the column with all missing data from seguen_2006
seguen_2006 <- subset(seguen_2006, select = -c(`SEGUENEGA|csps teonsgo`) )
# get health center populations sizes for Seguenega from 2006 to 2010 (usable data)
seguen_2006_population_size = populations_in_this_district(seguen_2006, "2006")
seguen_2007_population_size = populations_in_this_district(seguen_2007, "2007")
seguen_2008_population_size = populations_in_this_district(seguen_2008, "2008")
seguen_2009_population_size = populations_in_this_district(seguen_2009, "2009")
seguen_2010_population_size = populations_in_this_district(seguen_2010, "2010")


# get health center populations sizes for hounde from 2006 to 2010 (usable data)

# get health center populations sizes for Seguenega from 2006 to 2010 (usable data)
hounde_2004_population_size = populations_in_this_district(hounde_2004, "2004")
hounde_2006_population_size = populations_in_this_district(hounde_2006, "2006")
hounde_2007_population_size = populations_in_this_district(hounde_2007, "2007")
hounde_2008_population_size = populations_in_this_district(hounde_2008, "2008")
hounde_2009_population_size = populations_in_this_district(hounde_2009, "2009")
hounde_2010_population_size = populations_in_this_district(hounde_2010, "2010")

# get populations sizes for Lena health centers in 2006 and 2007
# first remove columns with lot's of NA's or missing data
lena_2006 <- subset(lena_2006, select = -c(`LENA|dispe/mate bossora`, `LENA|nra kadomba`) )
lena_2007 <- subset(lena_2007, select = -c(`LENA|nra kadomba`))

lena_2006_population_size = populations_in_this_district(lena_2006, "2006")
lena_2007_population_size = populations_in_this_district(lena_2007, "2007")

Kvigue_2008 <- subset(Kvigue_2008, select = -c(`KARANGASSO VIGUE|csps dan`, `KARANGASSO VIGUE|csps yegueresso`,`KARANGASSO VIGUE|dispe diosso`))

# get populations sizes for Lena health centers in 2008 and 2010
# first remove columns with lot's of NA's or missing data.
kvigue_2008_population_size = populations_in_this_district(Kvigue_2008, "2008")
kvigue_2010_population_size = populations_in_this_district(Kvigue_2010, "2010")
