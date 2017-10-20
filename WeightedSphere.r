
#The axis parallel hyper-ellipsoid is similar to function of De Jong. It is also known
#as the weighted sphere model. Function is continuous, convex and unimodal. It
#has the following general definition

#Minimu is 0 , obtained in 0,0


library(plotly)



# lattice 
x <- seq (-5.12, 5.12, length=100)
y <- seq (-5.12, 5.12, length=100)

points <- expand.grid(x,y)
#f(x) = sum for i= 1 to noumber of dim , i * x_i ^ 2
z <-  points$Var1^2 + (2* points$Var2^2)
points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matric for surface display with plotly
surf <- matrix(points$z, nrow = 100)
plot_ly(z=~surf) %>%
  add_surface( opacity= 0.85 )

dev.off()
