
cat("Attention!: cet algorithm peu tournera pendant 2h en moyene \n en raison du grand
    nombre de combinaison de valeur évaluer")

ptm <- proc.time() #get system time at start of exucution

## 2D bifurcation diagram
## vector of amplitudes (epsilon_a0) and baseline invasion rate values (a0)
param1.name ="epsilon_a" # choosing bifurcation parameter1
param2.name ="a0" # choosing bifurcation parameter2
param1.index = which(param1.name == names(PARMS_SICRS_harmonic_CI)) # getting index of bifurcation parameter
param2.index = which(param2.name == names(PARMS_SICRS_harmonic_CI)) # getting index of bifurcation parameter

param1.seq = seq(0.1,1,by=0.025) # bifurcation parameter values to loop through. Chose values such that
param2.seq = seq(0.012/year,0.396/year,by=0.00003) # bifurcation parameter values to loop through. Chose values such that

# 0.1*1e-02 to 1.2*1e-02 for endemic hyperendemic transition
# 1.3*1e-02 to 3.3*1e-02 for hyperdemic epidemic transition.


if (!file.exists("bifurc2d.RData")) {
  bifur_I2d <- matrix(nrow=length(param2.seq)*length(param1.seq),ncol=10*year)
  k <- 1
  for (i in 1:length(param2.seq)) { # looping through values of a0
    inits0 <- inits
    for (j in 1:length(param1.seq)) { # looping through values of epsilon_a
      cat(i,j,param2.seq[i],param1.seq[j],"\n")
      
      dtmp=sim.SCIRS_harmonic(inits0,param.loop_a0_epsilon_a,nd=50*year)
      
     
        bifur_I2d[k,]=tail(dtmp,10*year)[,"newI.Carrier"]
      
      #res[k,] <- dtmp[302:501,"logI"] # storing the I's
      
      k <- k+1
      inits0=tail(dtmp,1L)[,-c(1,6:10)]
      inits0=c(unlist(inits))
    }
    save("res",file="output/bifurc2d.RData")
  } # end for loop
}else{
  cat("bifurc2d.RData already exist, if you would like to re-run the program\n
      for generating the data for 2d bifurcation plotting again then delate or change file name.")
}# end if loop

runtime = proc.time() - ptm  # compute the runtime of this piece of code
runtime_minutes = runtime[3]/60 # converting runtime from seconds to minutes
cat("Le temps de calcul de ce code est estimé à: ",runtime_minutes, "minutes\n", "Soit: ",runtime[3], "Seconds")



## plotting 2D-bifurc

###
bifur_I2dr <- round(bifur_I2d*1e+05,2)
# Calculate periods (timing of weekly incidence peaks in years) for each
# row; meaning each pair of a0 and epsilon_a after model reach steaty state.
zz <- apply(bifur_I2dr,1,function(x) unique(diff(findPeaks(x,))/year)

param1.seq = seq(0.1,1,by=0.01) # seasonality amplitude
param2.seq = seq(0.001,0.033,by=0.001) # baseline/mean value of the invasion rate.

zmat <- matrix(zz,nrow=length(param1.seq))
bb <- expand.grid(epsilon_a=param1.seq,a0=param2.seq)
bb$zz<-zz

pdf("figs/bifurc2d.pdf")
library(colorspace) # for choosing the color palette to use for the plot
pal <- choose_palette() # will prompt a window to choose or set custom
# color palette.
pdf("figs/bifurc2d.pdf",width=7,height=5,pagecentre=T)
par(gfx)
filled.contour(
  param1.seq,param2.seq,zmat,
  las=1,
  levels=c(1:5),color.palette = pal,
  key.title=title(main="Period\n(Years)",cex.main=0.9),
  key.axes = axis(4, seq(1, 5, by = 1)),  # maybe also asp = 1

  plot.title = title(
    main = "Seasonality of invation rate ",cex.main=1.1,                
    ylab=bquote(paste("Mean Invasion Rate  ", (a[0]))),
    xlab=bquote(paste("Seasonality Amplitude ",(epsilon[a])))
  )
)

mtext(side = 1, line = 4, adj = 1, cex = .66,
      bquote(paste("Simulated a grid of 91 values of  " ,epsilon[a],
" and 33 values of ", a[0]," (3000 pairs)"))
)
dev.off()

##=================== 
# effect of changing seasonal amplitude (epsilon_a) on disease incidence pic depending on
# value of baseline invasion rate.
zz1<- apply(bifur_I2dr,1,function(x) findPeaks(x,))



