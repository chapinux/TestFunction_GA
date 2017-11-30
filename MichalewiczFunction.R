# The Michalewicz function is a multimodal test function (owns n! local optima).
# The parameter m defines the “steepness” of the valleys or edges. Larger m leads
# to more difficult search. For very large m the function behaves like a needle in
# the haystack (the function values for points in the space outside the narrow peaks
# give very little information on the location of the global optimum). Function has
# the following definition
# 
#   f(x) = -  sum for i = 1 to n sin(x[i]) *  (sin(i*x[i]^2 / Pi))^(2*m)
#
# It is usually set m = 10. Test area is usually restricted to hyphercube 0 ≤ x i ≤ PI,
# i = 1, . . . , n. The global minimum value has been approximated by f (x) =
#   −4.687 for n = 5 and by f (x) = −9.66 for n = 10. Respective optimal solutions
# are not given.

library(plotly)


# lattice 
x <- seq (0,pi , length=100)
y <- seq (0,pi, length=100)


x <- 0
y <- 0

m <-  10


points <- expand.grid(x,y)

z <- - sin(points$Var1)*(sin((points$Var1^2)/pi)^(2*m))  - sin(points$Var2)*(sin(((2*points$Var2)^2)/pi)^(2*m)) 


points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = 100)
plot_ly(z=~surf, showlegend=FALSE, showscale=TRUE) %>%
  add_surface( opacity= 0.9 ) 

dev.off()
