# R packages that need to be loaded
# installPackage() is my custom function
# unless the package is already installed, it will.
# data.table contains the rbindlist() function
# shape contains writelable() function for labeling figures/plot
# (a, b, c, etc..)
# lubridate contains functions usefull for time series manipulation
# gplots contain textplot() function to display text information in a graphic.
# define a custom function to install packages only if they are not already installed yet.

# install.packages('devtools')
# library('devtools')
# install_github("kingaa/pomp")

installPackage <- function(packageName) {
  #scheck if a package is installed then install it if not.
  #@ parmeter : a character representing the package name
  if (packageName %in% rownames(installed.packages()) ==  FALSE) {
    cat("\nCOSTUM WARNING : In case package installation fail, please make sure your computer is connected to internet.\n")
    install.packages(packageName, repos = "https://cran.univ-paris1.fr/")
  }else{
    cat("COSTUM MESSAGE: ", packageName, " package was already installed.\n")
  }
} # installPackage ends

packages <- c(
    "knitr",   
    "deSolve",
    "zoo",
    "shape",
    "lubridate",
    "lattice",
    "latticeExtra",
    "FME", 
    "hydroGOF",
    "ggplot2",
    "gplots", 
    "data.table", 
    "bbmle", 
    "XML", 
    "reshape2", 
    "plyr",
    "nloptr",
    "scales", 
    "ggthemes", 
    "gridExtra", 
    "gdata", 
    "extrafont",
    "httr",
    #packages for stockastic model simmulations
    "adaptivetau", 
    "pomp", 
    "devtools", 
    "lazyeval",
    "foreach",
    #=======
    "doMC", # for doing simulation on a multicore machine (exemple my macbook pro has 4 cores
    # which i can leverage to parallelize simulations especially when large number of itterations...)
    "fitR" # include very usefull functions for trajectories matching plots
    #useing ggplot facet and so on 
)

#install packages
lapply(packages, installPackage)
#load packages
lapply(packages, require, character.only = T)

cat("Necessary packages are now installed and loaded into the current envirement or workspace...\n")


