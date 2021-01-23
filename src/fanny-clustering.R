# Skrypt realizujący grupowanie rozmyte algorytmem FANNY. Eksperymenty przeprowadzane za pomocą skryptu pozwalają 
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
library(e1071)

# wczytanie danych
source("./dataPreparation.R")

# wybór losowych danych o zdefiniowanym rozmiarze
SAMPLE_RATIO <- 0.1
clusteringInput <- slice_sample(playersAttributesFinal, prop=SAMPLE_RATIO)
summary(clusteringInput)

# zakres liczby grup wynikowych podczas testów
minK = 2
maxK = 40

# funkcja przynależności
memb <- function(x) { (1/x) / sum(1/x) }

# rozmyty algorytm fanny z odległością euklidesową
print("Euclidean")

# macierze odległości dla pełnego i losowo dobranego, małego zbioru danych
euclideanDistanceMatrix <- dist(clusteringInput, method="Euclidean")
euclideanDistanceMatrixFull <- dist(playersAttributesFinal, method="Euclidean")

# alokacja tablic wynikowych
result.euclidean.fannys <- list()
result.euclidean.centroids <- list()
result.euclidean.memberships <- list()
result.euclidean.metrics <- list()

for(i in minK:maxK){
  # znalezienie początkowego grupowania
  result <- fanny(euclideanDistanceMatrix, i, diss=TRUE, memb.exp=1.1)
  result.euclidean.fannys[[i-minK+1]] <- result
  print(paste("Found ", i, "clusters"))
  
  # wyznaczenie rozmytych centroidów
  result.euclidean.centroids[[i-minK+1]] <- data.frame(matrix(ncol=ncol(playersAttributesFinal), nrow=0, dimnames=list(NULL, names(playersAttributesFinal))))
  for(j in 1:i){
    centroid <- lapply(clusteringInput, weighted.mean, w=result$membership[,j])
    result.euclidean.centroids[[i-minK+1]] <- rbind(result.euclidean.centroids[[i-minK+1]], as.data.frame(centroid))
  }
  print("Found fuzzy centroids")

  # find membership to all clusters for all examples
  distances <- dist(playersAttributesFinal, result.euclidean.centroids[[i-minK+1]], method="Euclidean")
  result.euclidean.memberships[[i-minK+1]] <- t(apply(distances, 1, memb))
  print(paste("Found cluster's membership for all examples"))
  
  # obliczenie metryk dla grupowania
  result.euclidean.metrics[i-minK+1] <- SIL.F(euclideanDistanceMatrixFull, result.euclidean.memberships[[i-minK+1]], distance=TRUE)
  print(paste("Evaluated metrics for", i, "clusters"))
}

# usuwanie zbędnych macierzy
rm(euclideanDistanceMatrix)
rm(euclideanDistanceMatrixFull)

# rozmyty algorytm fanny z odległością Minkowskiego z parametrem p = 3
print("Minkowski")

# macierze odległości dla pełnego i losowo dobranego, małego zbioru danych
minkowskiDistanceMatrix <- dist(clusteringInput, method="Minkowski", p=3)
minkowskiDistanceMatrixFull <- dist(playersAttributesFinal, method="Minkowski", p=3)

# alokacja tablic wynikowych
result.minkowski.fannys <- list()
result.minkowski.centroids <- list()
result.minkowski.memberships <- list()
result.minkowski.metrics <- list()

for(i in minK:maxK){
  # znalezienie początkowego grupowania
  result <- fanny(minkowskiDistanceMatrix, i, diss=TRUE, memb.exp=1.1)
  result.minkowski.fannys[[i-minK+1]] <- result
  print(paste("Found ", i, "clusters"))
  
  # wyznaczenie rozmytych centroidów
  result.minkowski.centroids[[i-minK+1]] <- data.frame(matrix(ncol=ncol(playersAttributesFinal), nrow=0, dimnames=list(NULL, names(playersAttributesFinal))))
  for(j in 1:i){
    centroid <- lapply(clusteringInput, weighted.mean, w=result$membership[,j])
    result.minkowski.centroids[[i-minK+1]] <- rbind(result.minkowski.centroids[[i-minK+1]], as.data.frame(centroid))
  }
  print("Found fuzzy centroids")
  
  # przypisanie wszystkich próbek do grup
  distances <- dist(playersAttributesFinal, result.minkowski.centroids[[i-minK+1]], method="Minkowski", p=3)
  result.minkowski.memberships[[i-minK+1]] <- t(apply(distances, 1, memb))
  print(paste("Found clusters for all examples"))
  
  # obliczenie metryk dla grupowania
  result.minkowski.metrics[i-minK+1] <- SIL.F(minkowskiDistanceMatrixFull, result.minkowski.memberships[[i-minK+1]], distance=TRUE)
  print(paste("Evaluated metrics for", i, "clusters"))
}

# usuwanie zbędnych macierzy
rm(minkowskiDistanceMatrix)
rm(minkowskiDistanceMatrixFull)

# rozmyty algorytm fanny z odległością Manhattan
print("Manhattan")

# macierze odległości dla pełnego i losowo dobranego, małego zbioru danych
manhattanDistanceMatrix <- dist(clusteringInput, method="Manhattan")
manhattanDistanceMatrixFull <- dist(playersAttributesFinal, method="Manhattan")

# alokacja tablic wynikowych
result.manhattan.fannys <- list()
result.manhattan.centroids <- list()
result.manhattan.memberships <- list()
result.manhattan.metrics <- list()

for(i in minK:maxK){
  # znalezienie początkowego grupowania
  result <- fanny(manhattanDistanceMatrix, i, diss=TRUE, memb.exp=1.1)
  result.manhattan.fannys[[i-minK+1]] <- result
  print(paste("Found ", i, "clusters"))
  
  # wyznaczenie rozmytych centroidów
  result.manhattan.centroids[[i-minK+1]] <- data.frame(matrix(ncol=ncol(playersAttributesFinal), nrow=0, dimnames=list(NULL, names(playersAttributesFinal))))
  for(j in 1:i){
    centroid <- lapply(clusteringInput, weighted.mean, w=result$membership[,j])
    result.manhattan.centroids[[i-minK+1]] <- rbind(result.manhattan.centroids[[i-minK+1]], as.data.frame(centroid))
  }
  print("Found fuzzy centroids")
  
  # przypisanie wszystkich próbek do grup
  distances <- dist(playersAttributesFinal, result.manhattan.centroids[[i-minK+1]], method="Manhattan")
  result.manhattan.memberships[[i-minK+1]] <- t(apply(distances, 1, memb))
  print(paste("Found clusters for all examples"))
  
  # obliczenie metryk dla grupowania
  result.manhattan.metrics[i-minK+1] <- SIL.F(manhattanDistanceMatrixFull, result.manhattan.memberships[[i-minK+1]], distance=TRUE)
  print(paste("Evaluated metrics for", i, "clusters"))
}

# usuwanie zbędnych macierzy
rm(manhattanDistanceMatrix)
rm(manhattanDistanceMatrixFull)

# rozmyty algorytm fanny z odległością korelacyjną
print("Correlation")

# macierze odległości dla pełnego i losowo dobranego, małego zbioru danych
correlationDistanceMatrix <- dist(clusteringInput, method="correlation")
correlationDistanceMatrixFull <- dist(playersAttributesFinal, method="correlation")

# alokacja tablic wynikowych
result.correlation.fannys <- list()
result.correlation.centroids <- list()
result.correlation.memberships <- list()
result.correlation.metrics <- list()

for(i in minK:maxK){
  # znalezienie początkowego grupowania
  result <- fanny(correlationDistanceMatrix, i, diss=TRUE, memb.exp=1.1)
  result.correlation.fannys[[i-minK+1]] <- result
  print(paste("Found ", i, "clusters"))
  
  # wyznaczenie rozmytych centroidów
  result.correlation.centroids[[i-minK+1]] <- data.frame(matrix(ncol=ncol(playersAttributesFinal), nrow=0, dimnames=list(NULL, names(playersAttributesFinal))))
  for(j in 1:i){
    centroid <- lapply(clusteringInput, weighted.mean, w=result$membership[,j])
    result.correlation.centroids[[i-minK+1]] <- rbind(result.correlation.centroids[[i-minK+1]], as.data.frame(centroid))
  }
  print("Found fuzzy centroids")
  
  # przypisanie wszystkich próbek do grup
  distances <- dist(playersAttributesFinal, result.correlation.centroids[[i-minK+1]], method="correlation")
  result.correlation.memberships[[i-minK+1]] <- t(apply(distances, 1, memb))
  print(paste("Found clusters for all examples"))
  
  # obliczenie metryk dla grupowania
  result.correlation.metrics[i-minK+1] <- SIL.F(correlationDistanceMatrixFull, result.correlation.memberships[[i-minK+1]], distance=TRUE)
  print(paste("Evaluated metrics for", i, "clusters"))
}

# usuwanie zbędnych macierzy
rm(correlationDistanceMatrix)
rm(correlationDistanceMatrixFull)

# zapis danych wynikowych w pliku .Rdata
save(result.correlation.fannys,
     result.correlation.memberships,
     result.correlation.centroids, 
     result.correlation.metrics,
     
     result.manhattan.fannys, 
     result.manhattan.memberships,
     result.manhattan.centroids, 
     result.manhattan.metrics,
     
     result.minkowski.fannys, 
     result.minkowski.memberships, 
     result.minkowski.centroids,
     result.minkowski.metrics,
     
     result.euclidean.fannys, 
     result.euclidean.memberships, 
     result.euclidean.centroids, 
     result.euclidean.metrics,
     
     clusteringInput, 
     file="./results/fanny-results-full.RData", version=2
)
