# computation of annual proportion (prevalence) of C.
timestep=7.024
new.carriers_a<-ts.SCIRS_CI_a[,9]*timestep

nbyears=49
prevCar=vector(length=nbyears)
for(i in 1:nbyears){
  print(ts.SCIRS_CI_a[(1+52*(i-1)),3])
  print(sum(new.carriers_a[(1+52*(i-1)):(52*i)]))
  prevCar[i]=ts.SCIRS_CI_a[(1+52*(i-1)),3]+sum(new.carriers_a[(1+52*(i-1)):(52*i)])

}
plot(prevCar,type="l", ylim=c(0,max(prevCar)))

## calculating the annual mean prevalence of C
propC.moyen.an = sum(prevCar)/nbyears

propC.moyen.an

# calculation of the yearly prevalence of disease.

nbyears=49
prevIll=vector(length=nbyears)
for(i in 1:nbyears){
  print(ts.SCIRS_CI_a[(1+52*(i-1)),4])
  print(sum(ts.SCIRS_CI_a[(1+52*(i-1)):(52*i),6]))
  prevIll[i]=ts.SCIRS_CI_a[(1+52*(i-1)),4]+sum(ts.SCIRS_CI_a[(1+52*(i-1)):(52*i),6])
  
}
plot(prevIll,type="l", ylim=c(0,max(prevIll)))

# computing the mean annual prevalence of disease
min(prevIll)*100
max(prevIll)*100
propI.moyen.an<-(sum(prevIll)/nbyears)*100


# computing the model for a range of parameter uniformly drawn between 0 and 1

my.func<-function(pA){
  w<-seq(0,1,0.1)
  pR<-pA+w
  return(pR)
}

my.func1<-function(pA){
  #w<-seq(0,1,0.1)
  pR<-pA+z
  return(pR)
}

my.func(10)
w<-seq(0,1,0.1)
pA=10

for(i in unique(z)){
  pR2=my.func1(pA)
  
}
pR2 




