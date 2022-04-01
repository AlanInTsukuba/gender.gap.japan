# DemingStephanMatrix.R
#
# Solve equations (23) in Deming, W. E., and Frederick F. Stephan. (1940) "On a Least Squares Adjustment of a Sampled Frequency 
#        Table when the Expected Marginal Totals are Known." 
#        The Annals of Mathematical Statistics, vol. 11, no. 4, pp. 427-444.
#
# Reference:
# https://cran.r-project.org/web/packages/matlib/vignettes/linear-equations.html
#
# If not yet installed
install.packages("matlib")

# load library
library(matlib)

citation("matlib")
#  Michael Friendly, John Fox and Phil Chalmers (2021). matlib: Matrix Functions
#  for Teaching and Learning Linear Algebra and Multivariate Statistics. R package
#  version 0.9.5. https://CRAN.R-project.org/package=matlib

A <- matrix(c(7413, -3549, -2354,
		-3549, 4441, -544,
		-2354, -544, 3129), 3, 3, byrow=TRUE)
colnames(A) <- paste0('x', 1:3)
b <- c(31.97, 23.56, -32.22)
showEqn(A,b)

##   7413*x1 - 3549*x2 - 2354*x3  =   31.97 
##  -3549*x1 + 4441*x2  - 544*x3  =   23.56 
##  -2354*x1  - 544*x2 + 3129*x3  =  -32.22 

# Are the equations consistent?
c( R(A), R(cbind(A,b)) )  # show ranks

##  [1] 3 3

all.equal( R(A), R(cbind(A,b)) ) # consistent?

##  [1] TRUE

# Solve for X.

solve(A,b)

##           x1          x2          x3 
##  0.011822067 0.014898033 0.001186857 

solve(A) %*% b

##            [,1]
##  x1 0.011822067
##  x2 0.014898033
##  x3 0.001186857

# clean up
rm(A,b)


