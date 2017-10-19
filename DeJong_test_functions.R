library(plotly)



# lattice 
x <- seq (-5.12, 5.12, length=100)
y<-x 

points <- expand.grid(x,y)
z <- x*x + y*y 
points <- cbind(points, z)
names(points) <- c("x", "y", "z")
nrow(points)


#put in matrix for surface display in plotly

plot_ly(points, x=~x, y=~y, z = ~z) %>%
  add_markers( opacity= 0.85)
