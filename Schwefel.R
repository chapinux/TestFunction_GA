
# An extension of the axis parallel hyper-ellipsoid is Schwefel’s function. With
# respect to the coordinate axes, this function produces rotated hyper-ellipsoids. It
# is continuous, convex and unimodal. Function has the following general definition



#Minimum is 0 , obtained in 0,0


library(plotly)



# lattice 
x <- seq (-65.53, 65.63, length=100)
y <- seq (-65.53, 65.63, length=100)
z <- seq (0, 0, length=100)


inputs <- cbind(x,y)
 
n=2
for( i in 1:n){
  for (j in 1:i){
    z <- z + inputs[,j]^2
  } 
}

z==zz

points <- expand.grid(x,y)
#f(x) = sum for i= 1 to number of dim , sum for j = 1 to i ,  x_i ^ 2
zz <-  points$Var1^2 + (points$Var1^2 + points$Var2^2)
points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 100)
plot_ly(z=~surf) %>%
  add_surface( opacity= 0.85 )

dev.off()
