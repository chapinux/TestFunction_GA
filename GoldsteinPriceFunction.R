# The Goldstein-Price function is a global optimization test function. It has only
# two variables and the following definition
# 
# f(x1, x2) = [1+(x1 + x2 + 1)^2*(19-14x1 + 3x1 ^ 2  - 14 x2 + 6x1x2 + 3 x2 ^ 2) ]
#         * [30 + (2x1 - 3x2) ^ 2 * (18-32x1 + 12 x1^2) + 48x2 - 36x1x2 + 27x2^2  ]
# 
# Test area is usually restricted to the square −2 ≤ x1 ≤ 2, −2 ≤ x2 ≤ 2. Its global
# minimum equal f (x) = 3 is obtainable for (x1 , x2 ) = (0, −1).


library(plotly)


# lattice 
x <- seq (-2,2 , length=100)
y <- seq (-2,2, length=100)


points <- expand.grid(x,y)

z <- (1 + (points$Var1 + points$Var2 + 1 )^2 * (19-14*points$Var1 + 3*points$Var1^2 - 14*points$Var2 + 6 * points$Var1 * points$Var2 + 3* points$Var2^2) )*(30 + (2*points$Var1 - 3*points$Var2)^2 * (18- 32*points$Var1 + 12* points$Var2^2 + 48*points$Var2 - 36 * points$Var1 * points$Var2 + 27* points$Var2^2 ))


points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 100)
plot_ly(z=~surf, showlegend=FALSE, showscale=FALSE) %>%
  add_surface( opacity= 0.9 ) 
dev.off()

