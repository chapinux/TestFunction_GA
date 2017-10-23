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


# minimum value : 0.397887 (paper)
# minimum for 100x100 grid : 0.4010828

library(plotly)


# lattice 
x <- seq (-5,20 , length=100)
y <- seq (-5,20, length=100)


a <- 1 
b <- 5.1 / (4* pi^2)
c <- 5 / pi 
d <- 6
e <- 10
f <- 1 / (8*pi)


points <- expand.grid(x,y)

z <- a * ( points$Var2 -b*points$Var1^2 + c * points$Var1 - d)^2 + e* (1-f)*cos(points$Var1) + e

points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 100)
plot_ly(z=~surf, showlegend=FALSE, showscale=TRUE) %>%
  add_surface( opacity= 0.9 ) 
dev.off()
