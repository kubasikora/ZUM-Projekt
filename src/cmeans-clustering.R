# Skrypt realizujący grupowanie rozmyte algorytmem cmeans. Eksperymenty przeprowadzane za pomocą skryptu pozwalają 
# na uzyskanie danych opisujących wpływ wykorzystywanych różnych miar niepodobieństwa na jakość grupowania. Wynikiem 
# skryptu jest plik zawierający dane zawierające wartości miar jakości grupowania do dalszej analizy.

library(mltools)
library(cluster)
library(dplyr)
library(proxy)
library(clv)
library(clusterCrit)
library(fclust)
library(fpc)
library(ppclust)
library(e1071)

source("./dataPreparation.R")

# wybór losowych danych o zdefiniowanym rozmiarze
SAMPLE_RATIO <- 0.1
clusteringInput <- slice_sample(playersAttributesFinal, prop=SAMPLE_RATIO)
summary(clusteringInput)

minK = 2
maxK = 40

# algorytm cmeans z odległością euklidesową
print("Euclidean")

euclideanDistanceMatrixFull <- dist(playersAttributesFinal, method="Euclidean")
result.euclidean.ppclusts <- list()
result.euclidean.centroids <- list()
result.euclidean.memberships <- list()
result.euclidean.metrics <- list()

for(i in minK:maxK){
  # grupowanie cmeans
  result <- fcm(playersAttributesFinal, i, dmetric="euclidean", m=1.2)
  result.euclidean.ppclusts[[i-minK+1]] <- result
  result.euclidean.memberships[[i-minK+1]] <- result$u
  result.euclidean.centroids[[i-minK+1]] <- result$v
  
  print(paste("Found ", i, "clusters"))

  # obliczenie metryk
  result.euclidean.metrics[[i-minK+1]] <- SIL.F(euclideanDistanceMatrixFull, result.euclidean.memberships[[i-minK+1]], alpha=1, distance=TRUE)
  print(paste("Evaluated metrics for", i, "clusters"))
}

# usuwanie zbędnej macierzy
rm(euclideanDistanceMatrixFull)

# algorytm cmeans z odległością Minkowskiego
print("Minkowski")
minkowskiDistanceMatrixFull <- dist(playersAttributesFinal, method="Minkowski", p=3)
result.minkowski.ppclusts <- list()
result.minkowski.centroids <- list()
result.minkowski.memberships <- list()
result.minkowski.metrics <- list()

for(i in minK:maxK){
  # grupowanie cmeans
  result <- fcm(playersAttributesFinal, i, dmetric="minkowski", pw=3, m=1.2)
  result.minkowski.ppclusts[[i-minK+1]] <- result
  result.minkowski.memberships[[i-minK+1]] <- result$u
  result.minkowski.centroids[[i-minK+1]] <- result$v
  
  print(paste("Found ", i, "clusters"))
  
  # obliczenie metryk
  result.minkowski.metrics[[i-minK+1]] <- SIL.F(minkowskiDistanceMatrixFull, result.minkowski.memberships[[i-minK+1]], alpha=1, distance=TRUE)
  print(paste("Evaluated metrics for", i, "clusters"))
}

# usuwanie zbędnej macierzy
rm(minkowskiDistanceMatrixFull)

# algorytm cmeans z odległością Manhattan
print("Manhattan")
manhattanDistanceMatrixFull <- dist(playersAttributesFinal, method="Manhattan")
result.manhattan.ppclusts <- list()
result.manhattan.centroids <- list()
result.manhattan.memberships <- list()
result.manhattan.metrics <- list()

for(i in minK:maxK){
  # grupowanie cmeans
  result <- fcm(playersAttributesFinal, i, dmetric="manhattan")
  result.manhattan.ppclusts[[i-minK+1]] <- result
  result.manhattan.memberships[[i-minK+1]] <- result$u
  result.manhattan.centroids[[i-minK+1]] <- result$v
  
  print(paste("Found ", i, "clusters"))
  
  # obliczenie metryk
  result.manhattan.metrics[[i-minK+1]] <- SIL.F(manhattanDistanceMatrixFull, result.manhattan.memberships[[i-minK+1]], alpha=1, distance=TRUE)
  print(paste("Evaluated metrics for", i, "clusters"))
}

# usuwanie zbędnej macierzy
rm(manhattanDistanceMatrixFull)

# algorytm cmeans z odległością korelacyjną
print("Correlation")
correlationDistanceMatrixFull <- dist(playersAttributesFinal, method="correlation")
result.correlation.ppclusts <- list()
result.correlation.centroids <- list()
result.correlation.memberships <- list()
result.correlation.metrics <- list()

for(i in minK:maxK){
  # grupowanie cmeans
  result <- fcm(playersAttributesFinal, i, dmetric="correlation", m=1.2)
  result.correlation.ppclusts[[i-minK+1]] <- result
  result.correlation.memberships[[i-minK+1]] <- result$u
  result.correlation.centroids[[i-minK+1]] <- result$v
  
  print(paste("Found ", i, "clusters"))
  
  # obliczenie metryk
  result.correlation.metrics[[i-minK+1]] <- SIL.F(correlationDistanceMatrixFull, result.correlation.memberships[[i-minK+1]], alpha=1, distance=TRUE)
  print(paste("Evaluated metrics for", i, "clusters"))
}

# usuwanie zbędnej macierzy
rm(correlationDistanceMatrixFull)

# zapis danych wynikowych w pliku .Rdata
save(result.correlation.ppclusts, 
     result.correlation.centroids, 
     result.correlation.memberships,
     result.correlation.metrics,
     
     result.manhattan.ppclusts, 
     result.manhattan.centroids, 
     result.manhattan.memberships, 
     result.manhattan.metrics,
     
     result.minkowski.ppclusts, 
     result.minkowski.memberships, 
     result.minkowski.centroids,
     result.minkowski.metrics,
     
     result.euclidean.ppclusts, 
     result.euclidean.memberships, 
     result.euclidean.centroids, 
     result.euclidean.metrics,
     
     file="../results/cmeans-results-full.RData", version=2
)
