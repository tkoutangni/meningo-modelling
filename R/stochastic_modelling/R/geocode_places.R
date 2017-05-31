# install.packages("nominatim") # one of osm paskages installed for testing
# library(nominatim)

library("mapproj") # for map projections in R
library("ggmap") # contains a function to geocode from place name or adress
library("DeducerSpatial")
require("maps") # for plotting maps on R

# libraries photon for geocoding
# install.packages("digest") # needed to install photon package from github repo
library(digest) 
# devtools::install_github(repo = 'rCarto/photon')
library(photon)
# load raw_data_subset_sort which is the raw data set generated from the data-preparation script
seguenega_data_temp = subset(raw_data_subset_sort, district=="SEGUENEGA")
unique(seguenega_data_temp$fs) # see health centers in this district
unique(seguenega_data_temp$region.x) # verify the region name

# set locations names to geocode
seguen.locations = c("bema", "berenga", "gambo", 
              "goubré", "goungré", "irim", 
              "kalsaka","kossouka", "pourra",
              "rambo", "ramsa", "rondo", "zomkalaga", "kondé tangaye",
              "bouga", "kièblèga", "teonsgo", "koumda yargo")

# paste each village name with the country name to make geo search and geo coding 
# more precise.
vloc = lapply(locations, function(x){
        paste(x, 'Burkina Faso', sep = ", ")
        })
# unlist it and combine into a vector for the geocode function
vloc<-unlist(as.vector(vloc))

# A function to get long and lat using datasciencekit or google map as a source for location search and geocoding.
# Google tends to find less places in villages in Burkina-Faso
# As an open source project, Open street map tends to be able to locate and geocode those villages in Africa. 
# The "dsk" source seems to include other maps api like bing and osm etc...

get_lat_lon_dsk <-function(locations, source="dsk"){
        sapply(locations, function(x){
                as.numeric(geocode(paste(x, "Burkina faso", sep = ", "), source = source))
        })
}

# After several trials of different packages, I end up 
# using a package called "Photon".
# The geocode function in Photon package tends to find et geocode more places like villages in Africa which coordinates are available on Open Street Map but not on google. The packages also use elastic search to find places, which increase its tolerence to typos in adress

# A geocoding function based on the Photon package.
get_lat_lon_osm <-function(locations){
        library(photon)
        vloc = lapply(locations, function(x){
                paste(x, 'Burkina Faso', sep = ", ")
        })
        vloc<-unlist(as.vector(vloc)) 
        ## need to use the name space photon::geocode()
        ##  to avoid conflict with ggmap::geocode() function
        vloc_geocode = photon::geocode(vloc, limit = 1, key = "place")
        # affichage des résultats
        vloc_geocode[,c("location","name", "state", "country", "osm_key", "osm_value", "lon", "lat")]
}


locations_coordinates = get_lat_lon_osm(locations)

library(plyr)
distance_matrix <- rbind.fill(apply(subset(locations_coordinates, select=c("lon", "lat")), 1, function(x) print(x)))


distance_matrix <- rbind.fill(apply(subset(locations_coordinates, select=c("lon", "lat")), 1, function(x) print(paste(as.numeric(x)[1], as.numeric(x)[2], sep=" "))))

as.vector(distance_matrix)
data_coord = data.frame(from_coord = as.vector(distance_matrix),
           to_coord = as.vector(distance_matrix))
mapdist(from=as.character(data_coord$from_coord), to=as.character(data_coord$to_coord), mode = "driving")
#=========================================================
# install.packages("osmar") # r package to interact with openstreet map api 
# install.packages("tidyr") # for data frame manipulations
# require("tidyr")
# library("osmar")

# Another way of using Open street map api to geocode places
# src <- osmsource_api()
# seguen_health_centers_nodes = c("bema"= 3807532048,
#                                 )
# toto = get_osm(node(3807532048), source = src)
# toto_wide <- spread(toto$nodes$tags, k, v)
# 
# bb <- center_bbox(-1.53492, 53.81934, 500, 500)
# ctown <- get_osm(bb, source = src)
# plot(ctown)
# points(-1.53492, 53.81934, col = "red", lwd = 5)


# testing osm mapping function get_openstreetmap
# leftlat = -2.9581
# rightlat = -1.5024
# toplon = 14.3123
# bottomlon = 12.6162
#position of the north, burkina-faso on open street maps
get_openstreetmap(bbox= c(left = -2.9581,
                  bottom = 12.6162,
                  right = -1.5024,
                  top = 14.3123))


src <- osmsource_api()
bb = corner_bbox(left=-2.9581, bottom=12.6162, right=-1.5024, top=14.3123)
bb <- center_bbox(-2.2687, 13.4511, 1000, 1000)
ctown <- get_osm(bb, source = src)
plot(ctown)
points(-2.034417, 13.11540, col = "red", lwd = 20)
#qmplot(13.4511, -2.2687)

