# Shubert’s function
# This is a multimodal test function. The given form of function has only two va-
#   riables and the following definition
# # 
# Test area is usually restricted to the square −5.12 ≤ x 1 ≤ 5.12, −5.12 ≤ x 2 ≤
# 5.12.

library(plotly)

cadre <- 14
# lattice 
x <- seq (-cadre,cadre , length=100)
y <- seq (-cadre,cadre, length=100)
#every x,y couple
points <- expand.grid(x,y)

z <- seq(0,0, length =100)
z <-  0.5 + ( cos(sin(abs(points$Var1^2 + points$Var2^2)))^2 -0.5) / ((1+ 0.001*((points$Var1^2 + points$Var2^2))^2)^2)
#z <- z + 0.01*runif(length(z))


points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 100)
plot_ly(z=~surf, showlegend=FALSE, showscale=FALSE) %>%
  #add_surface( opacity= 0.9 ) 
  add_contour(ncontours=10)



twdev.off()
