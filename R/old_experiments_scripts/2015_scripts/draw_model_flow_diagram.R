library(diagram)

M  <- matrix(nrow = 4, ncol = 4, byrow = TRUE, data = 0)
C <- M
A <- M

M[2, 1] <- paste(expression(beta[0]))
M[3, 2] <- paste(expression(alpha))
M[4, 2] <- paste(expression(a[t]))
M[3, 4] <- paste(expression(rho))
M[1, 3] <- paste(expression(phi1))

C[2, 1] <- 0.0
C[3, 2] <- 0.0
C[4, 2] <- 0.0
C[3, 4] <- 0.0
C[1, 3] <- -0.07
A[2, 1] <- A[3, 2] <- A[3, 4] <- A[1, 3]<-A[4, 2]<-2
A[4, 1] <- 2

col   <- M
col[] <- "black"
col[4, 2] <- "blue"
plotmat(M, pos = c(1,1,2), curve = C, name = c("S","C","R","I"),
        box.size=c(0.05,0.05,0.05,0.05), box.prop = 1,
        arr.lwd=A,my=0.0,mx= 0.0, dtext = c(0.6),arr.length= 0.4,shadow.size = 0,
        lwd = 1, box.lwd = 2, box.cex = 1, cex.txt = 1, 
        arr.lcol = col, arr.col = col, box.type = "circle",
        lend=4)


##======
M  <- matrix(nrow = 4, ncol = 4, byrow = TRUE, data = 0)
C <- M
A <- M

M[2, 1] <- paste(expression(beta[t]))
M[3, 2] <- paste(expression(alpha))
M[4, 2] <- paste(expression(a[0]))
M[3, 4] <- paste(expression(rho))
M[1, 3] <- paste(expression(phi1))

C[2, 1] <- 0.0
C[3, 2] <- 0.0
C[4, 2] <- 0.0
C[3, 4] <- 0.0
C[1, 3] <- -0.07
A[2, 1] <- A[3, 2] <- A[3, 4] <- A[1, 3]<-A[4, 2]<-2
A[4, 1] <- 2

col   <- M
col[] <- "black"
col[2, 1] <- "blue"
plotmat(M, pos = c(1,1,2), curve = C, name = c("S","C","R","I"),
        box.size=c(0.05,0.05,0.05,0.05), box.prop = 1,
        arr.lwd=A,my=0.0,mx= 0.0, dtext = c(0.6),arr.length= 0.4,shadow.size = 0,
        lwd = 1, box.lwd = 2, box.cex = 1, cex.txt = 1, 
        arr.lcol = col, arr.col = col, box.type = "circle",
        lend=4)

##======
M  <- matrix(nrow = 4, ncol = 4, byrow = TRUE, data = 0)
C <- M
A <- M

M[2, 1] <- paste(expression(beta[t]))
M[3, 2] <- paste(expression(alpha))
M[4, 2] <- paste(expression(a[t]))
M[3, 4] <- paste(expression(rho))
M[1, 3] <- paste(expression(phi1))

C[2, 1] <- 0.0
C[3, 2] <- 0.0
C[4, 2] <- 0.0
C[3, 4] <- 0.0
C[1, 3] <- -0.07
A[2, 1] <- A[3, 2] <- A[3, 4] <- A[1, 3]<-A[4, 2]<-2
A[4, 1] <- 2

col   <- M
col[] <- "black"
col[2, 1] <- col[4, 2] <-"blue"
plotmat(M, pos = c(1,1,2), curve = C, name = c("S","C","R","I"),
        box.size=c(0.05,0.05,0.05,0.05), box.prop = 1,
        arr.lwd=A,my=0.0,mx= 0.0, dtext = c(0.6),arr.length= 0.4,shadow.size = 0,
        lwd = 1, box.lwd = 2, box.cex = 1, cex.txt = 1, 
        arr.lcol = col, arr.col = col, box.type = "circle",
        lend=4)

cat("\n Warning!!!: Not yet the complete model. \n 
    I need to complete this script to have birth and deaths flows shown on the diagram")
