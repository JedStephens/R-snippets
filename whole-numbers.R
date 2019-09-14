# Methods to check for whole i.e. integers.

# Simple method for Real numbers:
# Function by Tamas Papp
is.whole <- function(a) { floor(a)==a }

# An alternative suitable for Real numbers adapted from StackOverflow
is.whole <- function(a){(a - as.integer(a))==0}

# The following methods appropriate for Real & Complex numbers.
# Functions by Martin Maechler
# Source: https://stat.ethz.ch/pipermail/r-help/2003-April/032471.html
is.whole <- function(a) {
    (is.numeric(a) && floor(a)==a) ||
        (is.complex(a) && floor(Re(a)) == Re(a) && floor(Im(a)) == Im(a))
}

# An alternative by Martin Maechler
is.whole <- function(a, tol = 1e-7) {
    is.eq <- function(x,y) {
        r <- all.equal(x,y, tol=tol)
        is.logical(r) && r
    }
    (is.numeric(a) && is.eq(a, floor(a))) ||
        (is.complex(a) && {ri <- c(Re(a),Im(a)); is.eq(ri, floor(ri))})
}