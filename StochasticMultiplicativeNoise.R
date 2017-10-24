
# The variable \epsilon_i, (i=1,...,n) is a random variable uniformly distributed in [0, 1].
# 
# Here, n represents the number of dimensions and x_i \in [-5, 5] for i=1,...,n.

# f(x) =  sum for i = 1 to n esilon[i] * | x[i]  - 1 /i |

library(plotly)

# lattice 
x <- seq (-5,5, length=100) 
y <- seq (-5,5, length=100) 
#every x,y couple
points <- expand.grid(x,y)

z <- seq(0,0, length =100)


set.seed(42)

epsilon1 <- runif(length(z))
epsilon2 <- runif(length(z)) 

z <- epsilon1 * abs(points$Var1 - 1) + epsilon2 * abs(points$Var2 -1/2) 

points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 100)


plot_ly( z=~surf, showlegend=FALSE, showscale=FALSE, colors='Spectral') %>%
  add_surface( opacity= 0.9 ) 




dev.off()
