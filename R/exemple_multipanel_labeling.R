##################################################
#
# put_fig_letter_example.r   January 16, 2015
#
# Example use of the put.fig.letter() function in
# put_fig_letter.r script.
#
##################################################

# Source the legend function file
source("R/put.fig.letter.r")

# Set the seed
set.seed(1234)

# Generate a data point to plot
x <- matrix(rnorm(60), ncol=6)
y <- matrix(rnorm(60), ncol=6)

# Apply a random scale to each column
x <- apply(x, 2, function(x) x*runif(1)*10)
y <- apply(y, 2, function(x) x*runif(1)*10)

# Setup multiple plot regions
par(mfrow=c(2,3), mar=c(5,4,1.5,1)+0.1)

# You can feed an (x,y) location to put the figure
# letter if you like, or you can use a predefined
# location by name kind of like legend()
my.locations <- c("topleft", "topcenter", "topright",
                  "bottomleft", "bottomcenter", "bottomright")

# Make the plots and append a figure letter to each
# Note: put.fig.letter() sends additional parameters to
# the text() function.
for(i in 1:6) {
        plot(x[,i], y[,i], pch=16, xlab="x", ylab="y")
        my.label <- paste(letters[i], ".", sep="")
        put.fig.letter(label=my.label, location=my.locations[i], font=2)
}

# You can also pass put.fig.letter() an 'offset' that will
# adjust the location of the label by the (x,y) 'offset' fraction
# of the figure space.  The example below would place the label
# "a." in the topleft corner of the figure region with an offset
# of 10% of the figure space in the positive x (right) and negative
# y (down) direction.
# NOT RUN:
# put.fig.letter("a.", location="topleft", offset=c(0.1, -0.1))

# Caution: Long figure labels may run off the edge of the figure space.
# Use the 'offset' parameter for now to provide extra room.