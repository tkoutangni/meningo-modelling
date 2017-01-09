# new dataset obtain from maxime as of 17 june 2015.

# loading the data into my workspace. the data base has .csv extension and is located in data/raw_data folder
# currently my working directory is set to : getwd()

meningoData = read.csv(file="../data/raw_data/150415_bdd_FRFE.csv",header=TRUE,sep=",")
subsetBoulsa = subset(meningoData,district=="boulsa" & maladie=="menin")
subsetSeguenega = subset(meningoData,district=="seguenega" & maladie=="menin")
with(subsetBoulsa,plot(cas,type="l"),fsjoli=="csps_damkarko_ii")
with(subsetSeguenega,plot(convertToDate, cas, plot.type="single",type="l"))
subsetSeguenega$formatDate = as.Date(as.character(subsetSeguenega$date),"%Y-%m-%d")
subsetSeguenegaVar = subset(subsetSeguenega,select=c("district","fsjoli","date","cas","population","formatDate"))

toto = subset(subsetSeguenega,fsjoli=="csps_bema")
with(toto[year(toto$formatDate)==2006,], plot(formatDate,cas,type="b"))

with(subsetSeguenega[year(subsetSeguenega$formatDate)==2006,], plot(formatDate,cas,type="l"))


tata = function(x){
    numericDate = as.numeric(x)
    computeNumbDays = vector(mode="numeric", length=length(numericDate))
    for (i in 1:length(numericDate)){
            computeNumbDays[i] = numericDate[i+1]-numericDate[i]
        }
    return(computeNumbDays)
    
    }

zozo = tata(subsetSeguenega$formatDate)