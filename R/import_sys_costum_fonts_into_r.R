# This file should be sourced only once on a given installation of R. If R is reinstalled or updated it may be necessary to run this script again to import fonts into your current installation of R
install.packages("extrafont")

library(extrafont)
font_import()
#laad and register the fonts now into R
loadfonts( device = "pdf")
loadfonts(device="postscript")
# this is to be performed only once per R session
# go to http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html
# for more details about using costum fonts in ggplot and .ps files etc
