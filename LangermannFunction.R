# # The Langermann function is a multimodal test function. The local minima are
# # unevenly distributed. Function has the following definition
# 
# where (c i , i = 1, . . . , m), (a ij , j = 1, . . . , n, i = 1, . . . , m) are constant numbers
# fixed in advance. It is recommended to set m = 5.


# iterated over M indexes (usually 5)

#f(x) = sum for index =1 to 5  c[index] * exp ( (-1/pi) * sum for i = 1 to n (x[i]- a[i,j])^2  ) cos (pi * sum for i = 1 to n (x[i] - a[i,j])^2)

# minimum is -5.279 / -5.3 
#obtained for ? 

library(plotly)

# lattice 
x <- seq (0,10 , length=100)
y <- seq (0,10, length=100)
z <- seq (0,0, length=100)

coeffC <- c( 1, 2, 5, 2, 3)
coeffA <- c( 3, 5, 2, 1, 7)


points <- expand.grid(x,y)

for (i in 1:5){
z <- z +  coeffC[i] * exp((-1/pi) * ((points$Var1- coeffA[i])^2 + (points$Var2- coeffA[i])^2))* cos( pi *  ((points$Var1-coeffA[i])^2 + (points$Var2-coeffA[i])^2))
}
  

points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 1000)
plot_ly(z=~surf, showlegend=FALSE, showscale=TRUE) %>%
  add_surface( opacity= 0.85 ) 
dev.off()




