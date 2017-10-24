# “Drop wave” function
# This is a multimodal test function. The given form of function has only two va-
#   riables and the following definition
# 
# Test area is usually restricted to the square −5.12 ≤ x 1 ≤ 5.12, −5.12 ≤ x 2 ≤
# 5.12.


library(plotly)


# lattice 
x <- seq (-5.12,5.12 , length=100)
y <- seq (-5.12,5.12, length=100)


points <- expand.grid(x,y)

num <- - (1 + cos(12 * sqrt(points$Var1^2 + points$Var2^2 ))) 
deno <- 0.5 * (points$Var1^2 + points$Var2^2) +2
z <- num / deno

points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 100)
plot_ly(z=~surf, showlegend=FALSE, showscale=FALSE) %>%
  add_surface( opacity= 0.9 ) 
dev.off()
