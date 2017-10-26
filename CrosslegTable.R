# CrossLeg-Table test objective function.
# 
# This class defines the Cross-Leg-Table global optimization problem. 
#This is a multimodal minimization problem defined as follows:
#

# f(x) = -1 / ( abs(exp(abs(100-   (sqrt(x1^2  + x2^2))/pi  ))* sin x1 * sin x2 ) +1 )^0.1

# Global optimum: f(x_i) = -1. The global minimum is found on the planes x_1 = 0 and x_2 = 0

library(plotly)

# lattice 
nbsteps <- 100
x <- seq (-10,10, length=nbsteps) 
y <- seq (-10,10, length=nbsteps) 
#every x,y couple
points <- expand.grid(x,y)

z <- seq(0,0, length =nbsteps)


term <- abs(100 - (sqrt(points$Var1^2 + points$Var2^2)/pi)) 

deno <- exp(term)* sin(points$Var1) * sin(points$Var2)

z <-  -1 /  ((abs(deno) + 1)^0.1)

  
points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = nbsteps)


plot_ly( z=~surf, showlegend=FALSE, showscale=FALSE) %>%
  add_surface( opacity= 0.9 ) 
  #add_contour()
#available colorscales
#BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral



dev.off()
