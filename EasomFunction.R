# The Easom function is a unimodal test function, where the global minimum has a
# small area relative to the search space. The function was inverted for minimiza-
#   tion. It has only two variables and the following definition


# f(x1, x2) = -cos(x1)cos(x2)exp( -(x1-pi)^2 - (x2-pi)^2   )

# Test area is usually restricted to square −100 ≤ x 1 ≤ 100, −100 ≤ x 2 ≤ 100. Its
# global minimum equal f (x) = −1 is obtainable for (x1 , x2 ) = (pi, pi). 



library(plotly)


# lattice 
x <- seq (-100,100 , length=100)
y <- seq (-100,100, length=100)



points <- expand.grid(x,y)

z <- -cos(points$Var1) * cos(points$Var2)*exp( -(points$Var1 - pi)^2    - (points$Var2 - pi)^2    )
  
  
points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 100)
plot_ly(z=~surf, showlegend=FALSE, showscale=FALSE) %>%
  add_surface( opacity= 0.9 ) 
dev.off()

