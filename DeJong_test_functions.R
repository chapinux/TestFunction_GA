library(plotly)



# lattice 
x <- seq (-5.12, 5.12, length=100)
y <- seq (-5.12, 5.12, length=100)

points <- expand.grid(x,y)

z <-  points$Var1^2 + points$Var2^2
points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matric for surface display with plotly
surf <- matrix(points$z, nrow = 100)
plot_ly(z=~surf) %>%
  add_surface( opacity= 0.85 )

dev.off()
