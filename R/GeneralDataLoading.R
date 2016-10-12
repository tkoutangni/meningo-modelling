
#####Loading data 
# Preparing data for model parameters optimisation.
# subseting the data by district
# viewing the names of the columns in incidences.ts database.
dataBase = load("data/raw_data/data_fev_2015/141125_TS_cas_incidences.RData")
load("data/raw_data/data_fev_2015/141125_TS_cas_incidences.RData")
names(incidences.ts) # the database of meningitis is stored in "menin" a zoo multiple time serie
names(incidences.ts$menin)
incid.menin = incidences.ts$menin

# a little function to get the columns for each district using "grep" pattern matching function
district<-function(database=incid.menin, district.name=""){
    col.indice = grep(district.name,c(names(database)))
    return(col.indice = as.numeric(c(col.indice)))
}

district.index = list(
    boulsa     = district(district.name="BOULSA"),
    dafra      = district(district.name="DAFRA"),
    dande      = district(district.name="DANDE"),
    do         = district(district.name="DÔ"),
    gourcy     = district(district.name="GOURCY"),
    hounde     = district(district.name="HOUNDÉ"),
    k.vigue    = district(district.name="KARANGASSO VIGUE"),
    lena       = district(district.name="LENA"),
    orodora    = district(district.name="ORODARA"),
    ouahigouya = district(district.name="OUAHIGOUYA"),
    seguenega = district(district.name="SEGUENEGA"),
    yako       = district(district.name="YAKO")
)

#===================================================
# selection formations sanitaires ayant connu ou pas des foyer par district
#===================================================
# The following function find epidemic incidences and compute time_diff 
# between consecutive epidemic incidences. if time time_diff is a week then outbreak def is met.

find.outbreak = function(district.data,verbose=TRUE){
    # the parameter verbose is used to print the result of the find outbreak function
    # if set to FALSE then nothing prints to the console.(the function becomes useless)
    if(verbose){
        for (i in seq_along(names(district.data))){
            cat(names(district.data)[i],diff(time(district.data[,i][c(which(district.data[,i]>=75/1e+05))])),"\n ") 
        } # end for loop
    }## end if
} # end function

# function to group health centers into 2 groups depending of experience of outbreak or not 

group_fs = function(district.data,fe_fs=""){
    fe_fs_index<-which( # return indexes 
        names(district.data)%in%fe_fs
    )
    
    non_fe_fs<-setdiff( # returns column names (health centre names)
        names(district.data),
        fe_fs
    )
    
    non_fe_fs.index<-which( # return indexes
        names(district.data)%in%non_fe_fs
    )
    
    return(list(fe_fs_index=fe_fs_index,
                non_fe_fs=non_fe_fs,
                non_fe_fs.index=non_fe_fs.index)
    )
}





