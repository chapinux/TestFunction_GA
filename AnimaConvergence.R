library(ggplot2)
library(dplyr)




setwd("/home/paul/dev/TestFunction_GA/rastrigin_additive_gaussian/rastrigin_avg/")


# conditions de production des noisy :  stratégie autoadaptative
# une ligne = l'individu avec la fitness la plus basse qui a été répliqué 100 fois
# rastrigin : la valeur de la fonction prise en ce point génome
#fitness : moyenne des 100 fitness que l'individu a obtenu "avant"
# ça peut être le même un bon bout de temps (l'effort e calcul est réparti ailleurs dans la pop, on pas prédire le nombre d'évaluations qui aété consommé entre deux lignes de ces fichiers



###matrche pas !!!!
gatherData <- function(every ) {
  res <-    data.frame(runs = as.numeric(),
               replication = as.character(),
               rastrigin = as.numeric())
  files <- dir(getwd(), pattern = "*.csv")
  
  lapply(files, function(f) {
    cat(f, "\n")
    df <- read.csv(f)
    dd <- subset(df,
             (df$evaluations + 1) %% every == 0,
             select = c(evaluations, rastrigin))
    replicationNumber <- unlist(strsplit(unlist(strsplit(basename(f), ".csv")), "best"))[2]
    cat(replicationNumber, "\n")
    dd$replication <- replicationNumber
    res <- rbind(res, dd)
  })
  return(res)
  
}

setwd("/home/paul/dev/TestFunction_GA/rastrigin_additive_gaussian/rastrigin_noisy/")
resAggSTOCHA <- gatherData(1000)



# conditions de production: chaque génoem est évalué 100 fois , fitness = moyenne des 100 runs mais on s'en fout (c'est pour l'algo)
# rastrigin c'est la valeur de la fonction prise au point génome (g1,g2...g10)
setwd("/home/paul/dev/TestFunction_GA/rastrigin_additive_gaussian/rastrigin_avg/")
setwd("/home/paul/dev/TestFunction_GA/rastrigin_avg_10repli/")

f<-files[23]
resAggAVG <- gatherData(1)


p <- ggplot(resAggAVG, aes(evaluations, rastrigin)) +
  geom_boxplot(aes(group=evaluations))

p





estimateurs  <- resAggreg %>%
  group_by(evaluations) %>% 
  summarise(Mean=mean(rastrigin), Median= median(rastrigin))



ggplot(estimateurs, aes(evaluations)) + 
  geom_line(aes(y = Mean, colour = "Mean")) + 
  geom_line(aes(y = Median, colour = "Median"))






























## version de gather iterative 
for (f in dir(getwd(),pattern = "*.csv")){
  cat(f, "\n")
  df <- read.csv(f)
  dd <- subset(df, (df$evaluations)%%1000 ==0, select = c(evaluations, rastrigin))
  replicationNumber <- unlist(regmatches(f, gregexpr("[[:digit:]]+", f)))
  dd$replication <- as.character(replicationNumber)
  resAggAVG <- rbind(resAggAVG,dd)
}


dd <- read.csv(f)





#count des outliers 