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





