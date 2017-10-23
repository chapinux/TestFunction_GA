# The Six-hump camel back function is a global optimization test function. Wi-
#   thin the bounded region it owns six local minima, two of them are global ones.
# Function has only two variables and the following definition

# f(x1, x2) = (4 - 2.1 x1 ^2 + x1^4 / 3 )* x1^2 + x1x2 + (-4 + 4x2^2)x2^2 


# 
# Test area is usually restricted to the rectangle −3 ≤ x1 ≤ 3, −2 ≤ x2 ≤ 2. Two
# global minima equal f (x) = −1.0316 are located at (x1 , x2 ) = (−0.0898, 0.7126)
# and (0.0898, −0.7126).


library(plotly)


# lattice 
x <- seq (-2,2 , length=100)
y <- seq (-2,2, length=100)


points <- expand.grid(x,y)


z <- points$Var1^2 * (4 - 2.1 * points$Var1^2 + (points$Var1^4)/3 ) + points$Var1 * points$Var2 + (points$Var2^2) * (-4 + 4* (points$Var2)^2)


points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 100)
plot_ly(z=~surf, showlegend=FALSE, showscale=FALSE) %>%
  add_surface( opacity= 0.9 ) 
dev.off()
