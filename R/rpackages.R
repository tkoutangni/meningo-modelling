# R packages that need to be loaded
# installPackage() is my custom function
# unless the package is already installed, it will.
# data.table contains the rbindlist() function
# shape contains writelable() function for labeling figures/plot
# (a, b, c, etc..)
# lubridate contains functions usefull for time series manipulation
# gplots contain textplot() function to display text information in a graphic.

packages <- c(
    "deSolve","zoo","shape","lubridate","lattice","latticeExtra","FME", "hydroGOF","ggplot2","gplots", "data.table", "bbmle", "XML", "reshape2", "plyr","nloptr","scales", "ggthemes"
)

#install packages
lapply(packages, installPackage)

#load packages
lapply(packages, require, character.only = T)

cat('finished loading packages...\n')