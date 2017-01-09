# ploting the scirs model flow diagram in R
library(diagram)

.size <- 13
.visible <- c(2, 3, 5, 7)


##  Initiate the matrices:
.A  <- matrix(nrow = .size,
              ncol = .size)
.arr.lwd <- matrix(0, .size, .size)
.curve <-  matrix(0, .size, .size)
.col <- matrix("black", .size, .size)

##  Define the arguments:
.pos <- c(1, 1, 1, 5, 5)
.box.size <- rep(0.05, length = .size)
##  
.name <- rep(x = "", length = .size)
.name[.visible] <- c("S", "C", "R", "I")
##
.box.col <-  rep(x = "white", length = .size)
.box.lcol <- .box.col
.box.lcol[.visible] <- "black"
##  Arrows up from/down to top visible node:
.arr.lwd[2, 1] <- 2
.curve[2, 1] <- 0.05
.A[2, 1] <- "b"
.arr.lwd[1, 2] <- 2
.curve[1, 2] <- 0.05
.A[1, 2] <- "mu"
##  Arrow down from top visible node:
.arr.lwd[3, 2] <- 2
.A[3, 2] <- "beta[0]"
##  Arrows down from the second visible node (from top):
.arr.lwd[5, 3] <- 2
.A[5, 3] <- "alpha"
.arr.lwd[7, 3] <- 2
.A[7, 3] <- "a[t]"
.col[7, 3] <- "blue"
##  Arrows from the leftmost visible node:
.arr.lwd[4, 5] <- 2
.A[4, 5] <- "mu"
.arr.lwd[2, 5] <- 2
.curve[2, 5] <- -0.07
.A[2, 5] <-  "phi1"
##  Arrows from the rightmost visible node:
.arr.lwd[5, 7] <- 2
.A[5, 7] <- "rho"
.arr.lwd[8, 7] <- 2
.A[8, 7] <- "gamma"
.arr.lwd[12, 7] <- 2
.A[12, 7] <- "mu"

##  Adjustment of node `6` to remove "gap" from arrow:
.box.size[6] <- 0
.box.col[6] <- "black"
.box.lcol[6] <- "black"


##  An argument to allow fine-tuning of the arrowhead-positions
##  related to "empty" nodes:
.arr.pos <- matrix(0.5, .size, .size)
.empty_places_top <- rbind(
  c(1, 2),
  c(2, 1))
.empty_places_bottom <- rbind(
  c(4, 5),
  c(8, 7),
  c(12, 7))
.arr.pos[.empty_places_top] <- 0.58
.arr.pos[.empty_places_bottom] <- 0.66


##  Create the desired plot.
plotmat(A = .A,
        pos = .pos,
        curve = .curve,
        name = .name,
        box.size = .box.size, 
        box.col = .box.col,
        box.lcol = .box.lcol,
        box.prop = 1,
        arr.lwd = .arr.lwd,
        my = 0.0,
        mx = 0.0,
        dtext = c(0.6),
        arr.type = "triangle",
        arr.pos = .arr.pos,
        arr.length= 0.4,
        shadow.size = 0,
        lwd = 1,
        box.cex = 1,
        cex.txt = 1, 
        arr.lcol = .col,
        arr.col = .col,
        box.type = "circle",
        lend = 4)