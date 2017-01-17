install.packages("extrafont")

library(extrafont)
font_import()
#laad and register the fonts now into R
loadfonts()
# this is to be performed only once per R session
# go to http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html
# for more details about using costum fonts in ggplot and .ps files etc
