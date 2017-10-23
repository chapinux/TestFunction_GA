# Schwefel’s function is deceptive in that the global minimum is geometrically di-
#   stant, over the parameter space, from the next best local minima. Therefore, the
# search algorithms are potentially prone to convergence in the wrong direction.

#Its global minimum f (x) = −418.9829 * n is obtainable for x i = 420.9687


# this implementation of the function produces a min of -418.58 (2D case, 100 values per axis)

# f(x) = soum for i = 1 to 1  -x[i] * sin(sqrt(abs(x[i])))




library(plotly)

# lattice 
x <- seq (-500,500 , length=100)
y <- seq (-500,500, length=100)


points <- expand.grid(x,y)

z <- -points$Var1 * sin ( sqrt(abs(points$Var1))) +   -points$Var2 * sin ( sqrt(abs(points$Var2)))
  
  
min(points$z)   /2 


points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 100)
plot_ly(z=~surf) %>%
  add_surface( opacity= 0.85 ) 

dev.off()
