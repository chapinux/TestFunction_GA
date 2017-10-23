# Griewangk’s function is similar to the function of Rastrigin. It has many wide-
#   spread local minima regularly distributed. Function has the following definition

# Test area is usually restricted to hyphercube −600 ≤ x i ≤ 600, i = 1, . . . , n. Its
# global minimum equal f (x) = 0 is obtainable for x i = 0, i = 1, . . . , n. The func-
#   tion interpretation changes with the scale; the general overview suggests convex
# function, medium-scale view suggests existence of local extremum, and finally
# zoom on the details indicates complex structure of numerous local extremum.


# function is defined as

#  f(x) = 1/4000  sum for i=1 to n x[i]^2 - product for i = 1 to n cos (x[i]/sqrt(i)) + 1

# N.B. Very sensitive to the scale of observation , local minima does not appear for -600;600 , 100 points per axis



library(plotly)

# lattice 
x <- seq (-300,300 , length=100)
y <- seq (-300,300, length=100)


points <- expand.grid(x,y)

 z <- ( 1/ 4000 ) * (points$Var1^2 + points$Var2^2 ) - (( cos(points$Var1) * cos(points$Var2 / sqrt(2)) + 1))


points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 100)
plot_ly(z=~surf) %>%
  add_surface( opacity= 0.85 ) 

dev.off()

