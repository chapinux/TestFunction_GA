# A deceptive problem is a class of problems in which the total size of the basins for
# local optimum solutions is much larger than the basin size of the global optimum
# solution.
# 

# f(x) = - (1/n *   sum for i = 1 to n g[i](x[i]) ) ^ beta

# where β is an fixed non-linearity factor.
# 
# A complex deceptive problem (Type III), in which
# the global optimum is located at x i = α i , where α i is a unique random number
# between 0 and 1 depending on the dimension i. To this aim the following form of
# auxiliary functions has been proposed
# 
# g[i](x[i]) =  -x/ alpha[i] + 4/5    if     0 ≤ x[i] ≤ 4/5 alpha[i]
#             =  (5*x / alpha[i])  - 4   if      4/5 alpha[i] ≤ x[i] ≤ alpha[i]
#           =   1 +   5*(x - alpha[i]) / (alpha[i] -1 )   if   alpha[i]  ≤ x[i] ≤ 1 + 4 * alpha[i]) / 5 
#            = 4/5  + (x-1)/(1-alpha[i])  if   (1 + 4 * alpha[i]) / 5  ≤ x[i] ≤  1
 # 
# The two other types of deceptive problems (Types I and II) are special cases
# of the complex deceptive problem, with α i = 1 (Type I), or α i = 0 or 1 at random
# (Type II) for each dimension i, i = 0, . . . , n. Clearly formulae (22) should be
# suitable adjusted for type I and II.
# For all three types of g i (x i ), the region with local optima is 5 n − 1 times larger
# than the region with a global optimum in the n-dimensional space. The number
# of local optima is 2 n − 1 for Type I and Type II deceptive problems and 3 n − 1 for
# Type III.
# if beta = 2 , minimum obtained for x* = (alpha1, alpha 2) ?


# lattice 
nbsteps <- 100


beta <- 0.8
alpha1 <- 0.3
alpha2 <- 0.7


x <- seq(0, 1 , length=100)
y <- seq(0, 1 , length=100)


#every x,y couple
points <- expand.grid(x,y)

z <- seq(0,0, length =100)



f1 <- function(x, alpha) {
   return(  4/5 - x/alpha  )
}

f2 <- function(x, alpha) {
  return(  (5*x)/alpha - 4   )
}


f3 <- function(x, alpha) {
  return( 1 + 5*(x - alpha) / (alpha -1)   )
}

f4 <- function(x, alpha) {
  return(4/5 + (x -1)/(1 - alpha  ))
}



g <- function(X, alpha){
  
  if (between(X,0,4/5 * alpha )){
         return(f1(X,alpha ))
  }
  if (between(X,4/5 * alpha, alpha )){
    return(f2(X,alpha ))
    }
  
  if (between(X,alpha, (1 + 4*alpha)/5 )){
    return(f3(X, alpha))  
  }
  if (between(X,(1 + 4*alpha)/5,1 )){
    return(f4(X,alpha))
    }
}

z <- - (0.5 * unlist(lapply(points$Var1,g,alpha= alpha1)) + unlist(lapply(points$Var2,g,alpha=alpha2)) )^beta







points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = nbsteps)

library(plotly)

plot_ly( z=~surf, showlegend=FALSE, showscale=FALSE) %>%
  add_surface( opacity= 0.9 ) 
#add_contour()
#available colorscales
#BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral



dev.off()

