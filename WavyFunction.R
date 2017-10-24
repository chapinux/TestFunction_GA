# Wavy function
#
#n is dimension of input space


# f(x) 1 - 1/ n  sum for i ) 1 to n  cos (k*x[i]) exp(-x[i]^2 / 2 )

# Where, in this exercise, k = 10. The number of local minima is kn and (k + 1)n for odd and even k respectively.
# 
# Here, n represents the number of dimensions and x_i \in [-\pi, \pi] for i=1,2

library(plotly)

# lattice 
x <- seq (-pi,pi, length=100) 
y <- seq (-pi,pi, length=100) 
#every x,y couple
points <- expand.grid(x,y)

z <- seq(0,0, length =100)

k<- 10
z <- 1 - 1/2 * ((cos(points$Var1 * k )* exp(-(points$Var1^2)/2)) + (cos(points$Var2 * k )* exp(-(points$Var2^2)/2)))

points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 100)


plot_ly( z=~surf, showlegend=FALSE, showscale=FALSE, colors='Spectral') %>%
    add_surface( opacity= 0.9 ) 

  
  
  
dev.off()
