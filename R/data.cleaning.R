doPlot=FALSE
# creating a vector of date correspondind the calendar weeks were meningitis case were assumed reported
#calendar.weeks.date=ISOweek2date(paste0(df_kalsaka$annee,"-W",sprintf("%02d",df_kalsaka$semaine),"-1"))


if(doPlot) incid_plot( # plot all health centre in seguenegua
    incid.menin[,district.index$seguenega], 
    main=main.title("Seguenega"),
    sub="Health centers which experienced an outbreak"
)


#===================================================
# Formations sanitaire ayant depasser le seuil de 75/100,000 pendant deux semaines 
# avec au moins 5 cas par semaine (revoir definition car nb cas par semaine est dépendant 
# de la taille de la population donc de la taille des formations sanitaires.)
#===================================================


####  SEGUENEGA District 
# print the result of the  find.outbreak function to check if it actually worked
verbose = TRUE # important to set the value to either TRUE or FALSE to show or hide
# find.outbreak function result on the console.
find.outbreak(incid.menin[,district.index$seguenega],verbose=verbose)

# formation sanitaire avec foyer épidemique.
fe_fs_seguenega<-c( # formation sanitaire avec foyer épidemique.
    "SEGUENEGA|csps goungré",
    "SEGUENEGA|csps irim",
    "SEGUENEGA|csps kalsaka",
    "SEGUENEGA|csps pourra",
    "SEGUENEGA|csps rambo",
    "SEGUENEGA|csps ramsa",
    "SEGUENEGA|dispe kondé tangaye"
)

# xyplot(cas.ts$menin[,fe_fs_seguenega]) # check if number of cases is at least 5 per 
# week when treshhold crossed.



group_fs_seguenega = group_fs(incid.menin[,district.index$seguenega],fe_fs_seguenega) # seguenega h.centres

if(doPlot) incid_plot(incid.menin[,district.index$seguenega][,group_fs_seguenega$fe_fs_index],
           main="Seguenega District \n 
           Health Centres with at least an epidemic",sub="Dotted line : Epidemic treshold")



####  HOUNDE District 

find.outbreak(incid.menin[,district.index$hounde],verbose=verbose)

# formation sanitaire avec foyer épidemique.
fe_fs_hounde<-c( # formation sanitaire avec foyer épidemique.
    "HOUNDÉ|csps bassé",
    "HOUNDÉ|csps bohokari",
    "HOUNDÉ|csps bouahoun",
    "HOUNDÉ|csps bouéré",
    "HOUNDÉ|csps dahoun",
    "HOUNDÉ|csps dougoumanto ii",
    "HOUNDÉ|csps fafo",
    "HOUNDÉ|csps koumbia",
    "HOUNDÉ|csps sara"
)

group_fs_hounde = group_fs(incid.menin[,district.index$hounde],fe_fs_hounde)

if(doPlot) incid_plot(incid.menin[,district.index$hounde][,group_fs_hounde$fe_fs_index],
           main="Hounde District \n 
           Health Centres with at least an epidemic",sub="Dotted line : Epidemic treshold")

## BOULSA district

find.outbreak(incid.menin[,district.index$boulsa],verbose=verbose)
# formation sanitaire avec foyer épidemique.

fe_fs_boulsa<-c( # formation sanitaire avec foyer épidemique.
    "BOULSA|csps gargo",
    "BOULSA|csps taffogo",
    "BOULSA|csps tilga"
)

group_fs_boulsa= group_fs(incid.menin[,district.index$boulsa],fe_fs_boulsa)
if(doPlot) incid_plot(incid.menin[,district.index$boulsa][,group_fs_boulsa$fe_fs_index],
           main="Boulsa District \n 
           Health Centres with at least an epidemic",sub="Dotted line : Epidemic treshold")


# DAFRA District

find.outbreak(incid.menin[,district.index$dafra],verbose=verbose)
# formation sanitaire avec foyer épidemique.

fe_fs_dafra<-c( # formation sanitaire avec foyer épidemique.
    "DAFRA|cm/cma koko",
    "DAFRA|csps kotédougou",
    "DAFRA|csps kouentou"
)

group_fs_dafra= group_fs(incid.menin[,district.index$dafra],fe_fs_dafra)
if(doPlot) incid_plot(incid.menin[,district.index$dafra][,group_fs_dafra$fe_fs_index],
           main="Dafra District \n 
           Health Centres with at least an epidemic",sub="Dotted line : Epidemic treshold")


# DANDE District.


find.outbreak(incid.menin[,district.index$dande],verbose=verbose)
# formation sanitaire avec foyer épidemique.

fe_fs_dande<-c( # formation sanitaire avec foyer épidemique.
    "DANDE|csps boboura",
    "DANDE|csps kokoroba",
    "DANDE|csps korédéni",
    "DANDE|csps lahirasso",
    "DANDE|csps padéma"
)

group_fs_dande= group_fs(incid.menin[,district.index$dande],fe_fs_dande)
if(doPlot) incid_plot(incid.menin[,district.index$dande][,group_fs_dande$fe_fs_index],
           main="Dande District \n 
           Health Centres with at least an epidemic",sub="Dotted line : Epidemic treshold")


# DÔ District.


find.outbreak(incid.menin[,district.index$do],verbose=verbose)

# formation sanitaire avec foyer épidemique.

# Aucune formation sanitaire correspond à la définition de foyer epidemique dans ce district
group_fs_do = group_fs(incid.menin[,district.index$do])
if(doPlot) incid_plot(incid.menin[,district.index$do][,group_fs_do$non_fe_fs.index],
           main="Dô District \n 
           Health Centres",sub="Dotted line : Epidemic treshold")
# donnée non exploitable


# GOURCY District.


find.outbreak(incid.menin[,district.index$gourcy],verbose=verbose)
# formation sanitaire avec foyer épidemique.

fe_fs_gourcy<-c( # formation sanitaire avec foyer épidemique.
    "GOURCY|csps guiri-guiri",
    "GOURCY|csps kéra-douré",
    "GOURCY|csps koundouba",
    "GOURCY|csps rikiba",
    "GOURCY|csps tangaye"
)

group_fs_gourcy= group_fs(incid.menin[,district.index$gourcy],fe_fs_gourcy)
if(doPlot) incid_plot(incid.menin[,district.index$gourcy][,group_fs_gourcy$fe_fs_index],
           main="Gourcy District \n 
           Health Centres with at least an epidemic",sub="Dotted line : Epidemic treshold")


# K.VIGUE DISTRICT

find.outbreak(incid.menin[,district.index$k.vigue],verbose=verbose)

fe_fs_k.vigue<-c( # formation sanitaire avec foyer épidemique.
    "KARANGASSO VIGUE|csps k. vigue",
    "KARANGASSO VIGUE|csps poya",
    "KARANGASSO VIGUE|csps soumousso"
   
)

group_fs_k.vigue= group_fs(incid.menin[,district.index$k.vigue],fe_fs_k.vigue)
if(doPlot) incid_plot(incid.menin[,district.index$k.vigue][,group_fs_k.vigue$fe_fs_index],
           main="K.Vigue District \n 
           Health Centres with at least an epidemic",sub="Dotted line : Epidemic treshold")

# LENA DISTRICT.

find.outbreak(incid.menin[,district.index$lena],verbose=verbose)

fe_fs_lena<-c( # formation sanitaire avec foyer épidemique.
    "LENA|csps bah",
    "LENA|csps dorossiamasso",
    "LENA|csps fina",
    "LENA|csps kofila",
    "LENA|csps lena",
    "LENA|csps satiri",
    "LENA|dispe/mate ouerou"   
)

group_fs_lena= group_fs(incid.menin[,district.index$lena],fe_fs_lena)
if(doPlot) incid_plot(incid.menin[,district.index$lena][,group_fs_lena$fe_fs_index],
           main="Lena District \n 
           Health Centres with at least an epidemic",sub="Dotted line : Epidemic treshold")


# ORODORA District.
find.outbreak(incid.menin[,district.index$orodora],verbose=verbose)

fe_fs_orodora<-c( # formation sanitaire avec foyer épidemique.
    "ORODARA|csps badara",
    "ORODARA|csps banflagoué",
    "ORODARA|csps fadona",
    "ORODARA|csps morolaba",
    "ORODARA|csps niamberla",
    "ORODARA|csps sifarasso",
    "ORODARA|csps sindo"   
)

group_fs_orodora= group_fs(incid.menin[,district.index$orodora],fe_fs_orodora)
if(doPlot) incid_plot(incid.menin[,district.index$orodora][,group_fs_orodora$fe_fs_index],
           main="Orodora District \n 
           Health Centres with at least an epidemic",sub="Dotted line : Epidemic treshold")


# OUAHIGOUYA District 

find.outbreak(incid.menin[,district.index$ouahigouya],verbose=verbose)

# formation sanitaire avec foyer épidemique.

# Aucune formation sanitaire correspond à la définition de foyer epidemique dans ce district
group_fs_ouahigouya = group_fs(incid.menin[,district.index$ouahigouya])

if(doPlot) incid_plot(incid.menin[,district.index$ouahigouya][,group_fs_ouahigouya$non_fe_fs.index])
# Donnée non exploitable pour ce district



# YAKO District 
find.outbreak(incid.menin[,district.index$yako],verbose=verbose)

fe_fs_yako<-c( # formation sanitaire avec foyer épidemique.
    "YAKO|csps bouria",
    "YAKO|csps thébo"
     
)

group_fs_yako= group_fs(incid.menin[,district.index$yako],fe_fs_yako)

if(doPlot) incid_plot(incid.menin[,district.index$yako][,group_fs_yako$fe_fs_index],
           main="Yako District \n 
           Health Centres with at least an epidemic",sub="Dotted line : Epidemic treshold")
# donnée non exploitable pour yako


#pdf(file="../figs/plot-march/epidemic_health_centre_ts_overview.pdf")

#dev.off()

###==========

if(doPlot) pdf(file="../figs/plot-march/non_epidemic_health_centre_ts_overview.pdf")

if(doPlot) incid_plot(incid.menin[,district.index$seguenega][,group_fs_seguenega$non_fe_fs.index],
           main="Seguenega District \n 
           Health Centres with no epidemic",sub="Dotted line : Epidemic treshold")

if(doPlot) incid_plot(incid.menin[,district.index$boulsa][,group_fs_boulsa$non_fe_fs.index],
           main="Boulsa District \n 
           Health Centres with no epidemic",sub="Dotted line : Epidemic treshold")

if(doPlot) incid_plot(incid.menin[,district.index$dafra][,group_fs_dafra$non_fe_fs.index],
           main="Dafra District \n 
           Health Centres with no epidemic",sub="Dotted line : Epidemic treshold")

if(doPlot) incid_plot(incid.menin[,district.index$dande][,group_fs_dande$non_fe_fs.index],
           main="Dande District \n 
           Health Centres with no epidemic",sub="Dotted line : Epidemic treshold")

if(doPlot) incid_plot(incid.menin[,district.index$do][,group_fs_do$non_fe_fs.index],
           main="Dô District \n 
           Health Centres with no epidemic",sub="Dotted line : Epidemic treshold")

if(doPlot) incid_plot(incid.menin[,district.index$gourcy][,group_fs_gourcy$non_fe_fs.index],
           main="Gourcy District \n 
           Health Centres with no epidemic",sub="Dotted line : Epidemic treshold")


if(doPlot) incid_plot(incid.menin[,district.index$hounde][,group_fs_hounde$non_fe_fs.index],
           main="Hounde District \n 
           Health Centres with no epidemic",sub="Dotted line : Epidemic treshold")


if(doPlot) incid_plot(incid.menin[,district.index$k.vigue][,group_fs_k.vigue$non_fe_fs.index],
           main="K.Vigue District \n 
           Health Centres with no epidemic",sub="Dotted line : Epidemic treshold")


if(doPlot) incid_plot(incid.menin[,district.index$lena][,group_fs_lena$non_fe_fs.index],
           main="Lena District \n 
           Health Centres with no epidemic",sub="Dotted line : Epidemic treshold")



if(doPlot) incid_plot(incid.menin[,district.index$orodora][,group_fs_orodora$non_fe_fs.index],
           main="Orodora District \n 
           Health Centres with no epidemic",sub="Dotted line : Epidemic treshold")

# Ouahigouya et Yako District données non exploitable,

if(doPlot) dev.off()

## Seguenega
df.no.epi = incid.menin[,district.index$seguenega][,group_fs_seguenega$non_fe_fs.index]

df.epi = incid.menin[,district.index$seguenega][,group_fs_seguenega$fe_fs_index]

dfEpiSeguenega = incid.menin[,district.index$seguenega][,group_fs_seguenega$fe_fs_index]

# Hounde
dfNoEpiHounde = incid.menin[,district.index$hounde][,group_fs_hounde$non_fe_fs.index]
dfEpiHounde = incid.menin[,district.index$hounde][,group_fs_hounde$fe_fs_index]
# Lena
dfNoEpiLena = incid.menin[,district.index$lena][,group_fs_lena$non_fe_fs.index]
dfEpiLena = incid.menin[,district.index$lena][,group_fs_lena$fe_fs_index]
# Kvigué
dfNoEpiKvigue = incid.menin[,district.index$k.vigue][,group_fs_k.vigue$non_fe_fs.index]
dfEpiKvigue = incid.menin[,district.index$k.vigue][,group_fs_k.vigue$fe_fs_index]

# Ouahigouya

dfNoEpiOuahigouya = incid.menin[,district.index$ouahigouya][,group_fs_ouahigouya$non_fe_fs.index]
## No foyer epidemic according to Haoua Tall definition.
dfEpiOuahigouya = incid.menin[,district.index$ouahigouya][,group_fs_ouahigouya$fe_fs_index]


# boulsa
dfEpiboulsa = incid.menin[,district.index$boulsa][,group_fs_boulsa$fe_fs_index]

dfEpiDafra = incid.menin[,district.index$dafra][,group_fs_dafra$fe_fs_index][,2:3] # keep only epidemic incidence

dfEpiDande = incid.menin[,district.index$dande][,group_fs_dande$fe_fs_index][,2:3] # keep only epidemic incidence

## Do district has no foyerEpidemic
# dfEpiDo = incid.menin[,district.index$do][,group_fs_do$fe_fs_index]

dfEpiGourcy = incid.menin[,district.index$gourcy][,group_fs_gourcy$fe_fs_index]

## some of the hs are not in epidemics according to Halima def (voir en details)
dfEpiOrodora = incid.menin[,district.index$orodora][,group_fs_orodora$fe_fs_index]

## Aucune donnée utilisable pour Yako foyer epidemic.
dfEpiYako = incid.menin[,district.index$yako][,group_fs_yako$fe_fs_index]
## xyplot(dfEpiYako)

#====================================================================
#New data cleaning starting October, 2016 to health centers population sizes
#and case count to complement the incidence per 100,000 inhabitant database
#=====================================================================
#

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


