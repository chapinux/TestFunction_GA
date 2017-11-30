# Shubert’s function
# This is a multimodal test function. The given form of function has only two va-
#   riables and the following definition
# # 
# Test area is usually restricted to the square −5.12 ≤ x 1 ≤ 5.12, −5.12 ≤ x 2 ≤
# 5.12.

library(plotly)
nbpts <- 200
cadre <- 15
# lattice 
x <- seq (-cadre/6,cadre/6 , length=nbpts)
y <- seq (-cadre,cadre, length=nbpts)
#every x,y couple
points <- expand.grid(x,y)

z <- seq(0,0, length =nbpts)
z <-  - log((sin((cos(points$Var1) + cos(points$Var2))^2)^2 - cos((sin(points$Var1) + sin(points$Var2))^2)^2 + points$Var1)^2)
z <-  z  + 0.01*((points$Var1-1)^2+ (points$Var2 - 1)^2 )
z <- z * 2.5 + 200

points <- cbind(points, z)
names(points) <- c("x", "y", "z")

#take z as a matrix for surface display with plotly
surf <- matrix(points$z, nrow = nbpts)

p <- plot_ly(z=~surf, showlegend=FALSE, showscale=FALSE)%>%
  add_surface( opacity= 0.9 ) 

p

#cool drawing

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE,
  ticklen = 0,
  tickwidth = 0
)

p <- plot_ly(z=~surf, showlegend=FALSE, showscale=FALSE)%>%
add_contour(ncontours=40, line= list(width=0.1, color='white'))%>%
  layout(xaxis=ax, yaxis=ax)

p

rm(p)
dev.off()
