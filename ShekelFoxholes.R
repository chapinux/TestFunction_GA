# Shekel’s foxholes
# This is a multimodal test function. It has the following definition
# 
 
#for inputs in n dimensions  , and n times m constants a[i,j] and m constants c[i]
# f(x) = - sum for i  1 to m (sum for j = 1 to n  (x[j]- a[i,j]^2 + c[j]))^⁻1
# 
# where (c i , i = 1, . . . , m), (a ij , j = 1, . . . , n, i = 1, . . . , m) are constant numbers
# fixed in advance. It is recommended to set m = 30.
# function does not seem to be this difficult looking at the surface ....

library(plotly)
library(dplyr)

# lattice 
x <- seq (0,10, length=100)
y <- seq (0,10, length=100)
#every x,y couple
points <- expand.grid(x,y)

m <- 10

c <- 0.1 * c(1, 2, 2, 4, 4, 6, 3, 3, 5, 5)
a <- rbind( c(4.0, 1.0, 8.0, 6.0, 3.0, 2.0, 5.0, 8.0, 6.0, 3.0), c(4.0, 1.0, 8.0, 6.0, 7.0, 9.0, 3.0, 1.0, 3.0, 3.6))

z <- seq(0,0, length =100)
for ( i in 1:m){
  z <- z -  ((points$Var1 - a[1,i] )^2 + c[i] + (points$Var2 - a[2,i] )^2 + c[i]   )^-1  
}




points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 100)
plot_ly(z=~surf, showlegend=FALSE, showscale=FALSE) %>%
  add_surface( opacity= 0.9 ) 
dev.off()
