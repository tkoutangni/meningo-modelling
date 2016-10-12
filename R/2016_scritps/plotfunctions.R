# Graphical display parameters
# par(pch=20, las=1, cex=1)
# gfx1<-par(layout(matrix(c(1, 1, 1, 1), 2, 2, byrow = TRUE),widths=c(3, 3), heights=c(2, 2))) # 1 rows 1 column
# gfx2<-par(layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE),widths=c(3, 3), heights=c(2, 2))) # 2 rows 1 column
# gfx4<-par(layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE),widths=c(3, 3), heights=c(2, 2))) # 2 rows 2 column

### Plot function for reuse.

## writting plots to a pdf file

to.pdf <- function(expr, filename, ..., verbose=TRUE) {
    
    if ( verbose )
        cat(sprintf("Creating %s\n", filename))
    pdf(filename, ...)
    on.exit(dev.off())
    eval.parent(substitute(expr))
    
} # end of function

#========================================

## functin for plotting any of the variables from the model simulation againt time.
# model take as arguments x, y and evry_xyear.
# x represent the time vector and y the time dependant var to plot.
# evry_xyear represent the time interval to display x axis tickmarks


plot.var<-function(x,y,evry_xyear,lty=NULL){ # this function was used to plot model variables
    # when model returned daily values of variable and won't work for when model
    # return weekly values instead ( t_inc = week instead of t_inc = day)
    
    plot(x,y,type='l',xaxt="n",xlab="time (years)",ylab="",main="",yaxs="r",xaxs="r",las=2,lty=lty)
    t_range.year<-seq(0,length(x),by=year)
    t_range.year = t_range.year[-1]
    evry_xyear=evry_xyear
    t_xaxis = seq(0,length(t_range.year),by=evry_xyear)
    t_xaxis =t_xaxis[-1]
    axis(1,at=c(0,t_range.year[t_xaxis]), labels=c(0,t_xaxis))
      
} # end of function


## the plot.var_vs_t function is for plotting variables against time especially weekly weekly 
# incidences.
# function takes the following arguments:
# var.name is the variable to plot. must match the name of the colon of x to plot
# x a data frame as retruned by model simulation output.
# eq_time is the time at which it's assumed model has reached it's stable equilibrium
    # and must be in days. eg: 50*year
# everyx=5 is were ticks marks has to be placed on the time axis. defaut to evry 5 (years).
# lty is the line type of the plot.

plot.var_vs_t = function(var.name,x,eq_time=50*year,everyx=5,lty=NULL,per_pop=1e+05){ # function plot variables
    # for the model when model return weekly values instead of daily values
    
    eq_time = eq_time
    everyx = everyx
    x = subset(x, time>=eq_time)
    var.name = var.name
    plot(x$time, x[,var.name]*per_pop, 
         type = "l",
         lty=lty,
         xaxt="n",
         ylab="",
         main="",
         xlab="time (years)",
         #xaxs="i",
         #yaxs="i",
         las=2
         )
    
    x_seq = seq(eq_time,x[dim(x)[1],1], everyx*year) # time sequence starting
    # from equilibrium time.
    axis(1, at=x_seq, labels= seq(0,length.out = length(x_seq),by=everyx))
}

####============================================ 

### 1-Dimention bifurcation diagram
# function takes 2 arguments: 
# range.x is the range of the bifurcation parameter 
# range.y is the range of the vector containing the values of "I"s 
# at the start of each of the j last years of simulation

bifur1D<-function(mbifur,range.x,range.y){
    
    mbifur = mbifur
    range.x = range.x
    range.y = range.y
    
    plot(0,0,pch="",
         xlim = range.x,
         ylim = range.y, # I Can choose to plot log scale of I for clarity.
         main = "",
         sub  = "",
         xlab = "",
         ylab = "",
         title(ylab ="Annual I",line=4.0),
         las=1,
         cex.lab = 1
         
         #xlab=paste("Seasonality Amplitude,",param.name)
    ) ## end plot region
    ## start for loop for plotting the I's at the start of each year
    
    for (i in 1:length(param.seq)) {
        points(rep(param.seq[i], 10), 
               mbifur[i,],cex=0.0001) # plotting log(bifur_I) for clarity.  
    } ## for loop end here
    
} # end of function




## function to compute the sinusoidally forced rate of invasion
## it takes 2 arguments:
## the baseline disease invasion rate 'a0' and,
## the amplitude of seasonality of a0 the invasion rate
## if different than 0, the argument a0 must be in years not in days^-1

a0.harmonic<-function(a0,epsilon_a,teta){
    year=year
    a0=a0/year
    if(a0!=0){
        
    t=seq(1,3*year,1)
    a    = a0*(1+epsilon_a*cos(2*pi*(t-teta)/year))
    #a  = a0 * ((4.5+45*epsilon_a)*sin(2*pi*(t-teta)/year)+(5.5+45*epsilon_a))
    #a  = a0 * ((49.5*epsilon_a)*cos(2*pi*(t-teta)/year)+(1 + 49.5*epsilon_a))
    return(a)
    
    }else{
        
        return(cat("a0 is the meningococcus baseline invasion rate\n and must be > 0 .The current value of a0 is : ",a0))
    }
}

# computing the function a0.harmonic  and geeting value range of a(t) with the fold increase
# between baseline a0 and the maximum value of a(t)

at<-function(a0, epsilon_a,teta){
    a_t= a0.harmonic(a0,epsilon_a,teta) # This is the function defined above for computing the values of a(t).
    range(a_t); (range(a_t)[2]) /range(a_t)[1] ## getting the fold-increase in a0 due to seasonal forcing epsilon_a
    #cat("value range of a(t) is :",range(a_t), "which correspond to a ",range(a_t)[2]/range(a_t)[1], "fold-increase")
    at = list(a_t=a_t, range = range(a_t))
    return(at)
    }


##=====================================

## Function for sinusoidal variations of carriage ( 1 to 5 times variation of beta0)

beta0.harmonic<-function(beta0,epsilon_b,teta){
    year=year
    beta0=beta0/year
    if(beta0!=0){
        
        t=seq(1,3*year,1)
        beta    = beta0*(1+epsilon_b*cos(2*pi*(t-teta)/year)) # beta is unforced at all.
        # variation de 1 a 2 quand epsilon_b = 0 et entre 2 et 4 quand epsilon_b=1
        #beta  = beta0 * ((0.5+0.5*epsilon_b)*cos(2*pi*(t-teta)/year)+(1.5+0.5*epsilon_b)) # b is forced with a minimum 2*fold increase of beta0 when epsilon=0
        
        return(beta)
        
    }else{
        return(cat("beta0 is the meningococcus mean transmission rate\n and must be > 0 .The current value of beta0 is : ",beta0))
        
    }
}



betat<-function(beta0, epsilon_b,teta){ 
    beta_t= beta0.harmonic(beta0,epsilon_b,teta) # This is the function defined above for computing the values of a(t).
    range(beta_t); (range(beta_t)[2]) /range(beta_t)[1] ## getting the fold-increase in a0 due to seasonal forcing epsilon_a
    #cat("value range of beta(t) is :",range(beta_t), "which correspond to a ",range(beta_t)[2]/range(beta_t)[1], "fold-increase\n")
    betat = list(beta_t=beta_t, range = range(beta_t))
    return(betat)
    }


##==========================================================================
# fontion to plot lattice graph of health centers data for a given district

#plotting function
incid_plot<-function(x, main="", ylim=NULL, sub=""){
    xyplot(x*1e+05, 
           pch=16,type="b",ylim=ylim,main=main,cex=.7,col="black",
           sub=sub,layout=c(3,3),ylab="Weekly Incidence \n per 100,000",
           #group = c(group1,group2), col=c("red","blue"),
           panel = function( x,y,...) {
               
               panel.abline( h=75, lty = "dotted", col = "black")
               
               panel.xyplot( x,y,...)
           })   
} 

# defining graphical parameters of the districts.

main.title<-function(district.name="------"){ #  argument must be of character class
    titre=paste(
        "Weekly incidence/100,000 \n Sanitary District of",
        district.name, "(Burkina-Faso) \n Dotted line represent treshold for localized outbreak"
    )
    titre
}

sub.title<-function(district.name="------"){ #  argument must be of character class
    titre=paste(
        "Health Centers in the sanitary District of",
        district.name, "(Burkina-Faso)"
    )
    titre
}

##===============================================================================

# Function to plot the mean weekly incidence data and  the 95% CI at health centre level
# takes dataframe as argument
plot.mean.weekly<-function(df,sub="",title=""){
    title=title
    sub = sub # subtitle of the plot
    toto = ci_95(df) # output mean as well as 95% CI around the mean
    max.y = max(toto[,"hbound"][which(!is.nan(toto[,"hbound"]))])
    min.y = max(toto[,"lbound"][which(!is.nan(toto[,"lbound"]))])
    plot(time(df), row.mean(df)*1e+05,type="n",lwd=2,xlab="",ylab="Average Weekly \n Incidence",las=1, ylim=c(min.y,1.2*(max.y*1e+05)))
    num_date = as.numeric(time(df))
    #polygon(c(rev(num_date), num_date), c(rev(toto[,"hbound"]*1e+05), toto[,"lbound"]*1e+05), col = 'grey75', border = NA)
    lines(time(df), toto[,2]*1e+05,col="black",lwd=2,lty=1)
    lines(time(df), toto[,"hbound"]*1e+05,col=2,lwd=2,lty=3)
    lines(time(df), toto[,"lbound"]*1e+05,col=3,lwd=2,lty=3)
    # shading the 95% ci area,
    
    mtext(expression(paste("x",10^-5)),side=3,adj=0, cex=0.8)
    title(main=title,sub=sub,cex.main=1)
    legend("topright",legend=c("Average","Upper 95% CI","Lower 95% CI"),col=c(1,2,3),lty=c(1,3,3),lwd=3,bty="n",cex=0.7)
    #legend("topright",legend=c("Average","95% CI"),col=c("black","grey75"),lty=c(1),lwd=3,bty="n",cex=0.7) 
}


##===

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multi_ggplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}


















##====          ============

# placing legend above graph
#legend("topright",
       #inset = c(0, -0.2),  x.intersp=0.1, y.intersp=-.5,bty="n",
       #xjust=0,yjust=0,lty=c(1,2,3),col=c("black","black","black"),
       #lwd=2, cex = 0.75, xpd = TRUE, horiz=T,
       #legend=c(expression(epsilon[a]==1,epsilon[a]==0,epsilon[a]==0.11)))








