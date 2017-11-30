# Fifth function of De Jong
# This is a multimodal test function. The given form of function has only two va-
#   riables and the following definition
# 
# i is the ith  dimension on input
# j is an int , j=25

# where a[i,j] = ( -32 -16 0 16 32 -32 -16 .... 0 16 32 
#                 -32 -32 -32 -32 -32 -16 -16 ... 32 32 32 32 32   )
# 
#   if i = 1 to 2 (2 inputs)  j = 25 , aij is a 2,25 matrix

# f(x1, x2) = (0.002 + sum for j = 1 to 25  (j + (x1 - a[1,j])^6 + (x2 - a[2,j])^6    )^-1 )^-1

# The function can also be rewritten as follows
#

#  f(x1, x2) = (0.002 + sum for i = -2 for j= -2 to 2 (5(i + 2)+j + 3 + (x1-16j)^6 + (x2 -16i)^6  )^-1  )^-1

# Test area is usually restricted to the square −65.536 ≤ x 1 ≤ 65.536, −65.536 ≤
# x 2 ≤ 65.536.


library(plotly)


# lattice 
x <- seq (-65.536,65.536 , length=100)
x <- seq (-65.536,65.536 , length=100)


x <-  16.0
y <- 32.0

firstAline <- rep(c(-32, -16, 0 , 16 , 32), 5)
secondAline <- c(rep(-32, 5), rep(-16,5), rep(0,5), rep (16, 5), rep(32,5))

A <- rbind(firstAline, secondAline)

points <- expand.grid(x,y)

z<-0
z <- seq(0,0, length= 100)
  
for(j in 1:25){
   z <- z + 1/(j + (points$Var1 - A[1,j])^6 + (points$Var2 - A[2,j])^6) 
}  
z <- (0.002 + z)^-1


points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 100)
plot_ly(z=~surf, showlegend=FALSE, showscale=FALSE) %>%
  add_surface( opacity= 0.9 ) 
dev.off()
