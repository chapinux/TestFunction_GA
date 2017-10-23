# The Branin function is a global optimization test function having only two va-
#   riables. The function has three equal-sized global optima, and has the following
# definition
# 
# 
# t is recommended to set the following values of parameters: 
# a = 1, b =  5.1 / (4 * pi ^2)
# c= 5 * pi 
# d = 6
# e = 10, 
#f = 1 / 8 * pi . Three global optima equal f (x 1 , x 2 ) = 0.397887
# are located as follows: (x 1 , x 2 ) = (−pi, 12.275), (pi, 2.275), (9.42478, 2.475).

library(plotly)


# lattice 
x <- seq (-10,10 , length=100)
y <- seq (-10,10, length=100)
z <- seq (0,0, length=100)

m <-  10


points <- expand.grid(x,y)

z <- - sin(points$Var1)*(sin((points$Var1^2)/pi)^(2*m))  - sin(points$Var2)*(sin(((2*points$Var2)^2)/pi)^(2*m)) 


points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 100)
plot_ly(z=~surf, showlegend=FALSE, showscale=TRUE) %>%
  add_surface( opacity= 0.9 ) 

dev.off()
