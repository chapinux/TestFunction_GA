# 
# Rastrigin’s function is based on the function of De Jong with the addition of cosine
# modulation in order to produce frequent local minima. Thus, the test function is
# highly multimodal. However, the location of the minima are regularly distributed.

# Function has the following definition


#f(x) = 10 * n  + sum for i = 1 to n  x[i]^2  - 10 * cos (2*Pi * x[i])
 #minimum is obtained for x[i] = 0 and is 0 s



library(plotly)

# lattice 
x <- seq (-5.12,5.12 , length=100)
y <- seq (-5.12,5.12, length=100)


points <- expand.grid(x,y)

z <- 10*2 +   (points$Var1^2  - 10* cos ( 2* pi * points$Var1) )  + (points$Var2^2  - 10* cos ( 2* pi * points$Var2)) 

points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 100)
plot_ly(z=~surf) %>%
  add_surface( opacity= 0.85 ) 

dev.off()






