# Rosenbrock’s valley is a classic optimization problem, also known as banana
# function or the second function of De Jong. The global optimum lays inside a
# long, narrow, parabolic shaped flat valley. To find the valley is trivial, however
# convergence to the global optimum is difficult and hence this problem has been
# frequently used to test the performance of optimization algorithms. Function has
# the following definition


#function expression : 
 # sum for i from 1 to n-1  100 * ( x[i+1] - x[i]^2 )^2 + (1-x[i])^2 

# minimum valoues is 0 obtaines for x1 = -+ 1 ???
library(plotly)

# lattice 
x <- seq (-2.04, 2.04, length=100)
y <- seq (-2.04,2.04, length=100)


points <- expand.grid(x,y)

z <- 100 *(points$Var2 - points$Var1^2)^2 + (1- points$Var1)^2

points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 100)
plot_ly(z=~surf) %>%
  add_surface( opacity= 0.85 ) 

dev.off()






