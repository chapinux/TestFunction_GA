library(plotly)
library(webshot)
setwd("/home/paul/dev/TestFunction_GA/dropwave/")



fifi <- file("0.csv")
df <- read.csv(fifi)

 names(df) <- c("x1", "x2", "fitness")
 df$fitness
 
 
 
 scene = list(
   xaxis = list( range= c(-2, 2), nticks=11),
   yaxis = list( range= c(-2, 2),nticks=11),
   zaxis = list( range = c(-1,0),nticks=5)
 )
 

 
 df$fitness
 


p<-plot_ly(df, x=~x1, y=~x2, z=~fitness, type = "scatter3d",
         mode= "markers",
         color="orange",
          
   marker= list(
     symbol="circle",
     size = 2,
     opacity = 0.8
   )
   )  %>%
  layout( scene = scene )

p 
tmpFile <- tempfile(fileext = ".png")
export(p, file = tmpFile)


length

