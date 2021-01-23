# Skrypt realizujący eksperymenty wykorzystujące algorytm hierarchiczny w wersji deglomeracyjnej. Eksperymenty 
# przeprowadzane za pomocą skryptu pozwalają na uzyskanie danych opisujących wpływ wykorzystywanych różnych miar 
# niepodobieństwa na jakość grupowania. Wynikiem skryptu jest plik zawierający dane zawierające wartości miar 
# jakości grupowania do dalszej analizy.

library(mltools)
library(cluster)
library(dplyr)
library(proxy)
library(clv)
library(clusterCrit)
library(fpc)

# wczytanie danych
source("./dataPreparation.R")

# wybór losowych danych o zdefiniowanym rozmiarze
SAMPLE_RATIO <- 0.1
clusteringInput <- slice_sample(playersAttributesFinal, prop=SAMPLE_RATIO)
summary(clusteringInput)

# zakres liczby grup wynikowych podczas testów
minK = 2
maxK = 40

##########################################################################################

# macierze odległości dla pełnego i losowo dobranego, małego zbioru danych
euclideanDistanceMatrix <- dist(clusteringInput, method="Euclidean")
euclideanDistanceMatrixFull <- dist(playersAttributesFinal, method="Euclidean")

# alokacja tablic wynikowych
result.euclidean.centroids <- list()
result.euclidean.clustering <- list()
result.euclidean.metrics <- list()
result.euclidean.compare.outfield <- list()
result.euclidean.compare.general <- list()
result.euclidean.compare.specific <- list()

print("===========================================================================")
print("Euclidean")
print("===========================================================================")

# wywołanie funkcji realizującej grupowanie deglomeracyjne
result <- diana(euclideanDistanceMatrix, diss=TRUE)

# pętla realizująca grupowanie dla różnych wartości k przy wykorzystaniu ogległości euklidesowej
for (k in minK:maxK) {
  # znalezienie początkowego grupowania w zależności od k
  clustering <- cutree(result, k=k)
  print(paste("Found ", k, "clusters"))
  
  # znalezienie środków wyznaczonych grup
  idx <- (clustering == 1)
  centroids <- colMeans(clusteringInput[idx,])
  for (i in 2:k) {
    idx <- (clustering == i)
    centroids <- rbind(centroids, colMeans(clusteringInput[idx,]))
  }
  print(paste("Found diana centroids"))
  
  distances <- dist(playersAttributesFinal, centroids, method="Euclidean")
  result.euclidean.centroids[[k-minK+1]] <- centroids
  
  # przypisanie wszystkich próbek do grup
  result.euclidean.clustering[[k-minK+1]] <- apply(distances, 1, which.min)
  print(paste("Found clusters for all examples"))
  
  # obliczenie metryk dla grupowania
  result.euclidean.metrics[[k-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering[[k-minK+1]])
  result.euclidean.compare.outfield[[k-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering[[k-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
  result.euclidean.compare.general[[k-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering[[k-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
  result.euclidean.compare.specific[[k-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering[[k-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
  
  print(paste("Evaluated metrics for", k, "clusters"))
  print("===========================================================================")
}

# usuwanie zbędnych macierzy
rm(euclideanDistanceMatrix)
rm(euclideanDistanceMatrixFull)

##########################################################################################

# macierze odległości dla pełnego i losowo dobranego, małego zbioru danych
manhattanDistanceMatrix <- dist(clusteringInput, method="Manhattan")
manhattanDistanceMatrixFull <- dist(playersAttributesFinal, method="Manhattan")

result.manhattan.centroids <- list()
result.manhattan.clustering <- list()
result.manhattan.metrics <- list()
result.manhattan.compare.outfield <- list()
result.manhattan.compare.general <- list()
result.manhattan.compare.specific <- list()

print("===========================================================================")
print("Manhattan")
print("===========================================================================")

# wywołanie funkcji realizującej grupowanie deglomeracyjne
result <- diana(manhattanDistanceMatrix, diss=TRUE)

# pętla realizująca grupowanie dla różnych wartości k przy wykorzystaniu ogległości Manhattan
for (k in minK:maxK) {
  # znalezienie początkowego grupowania w zależności od k
  clustering <- cutree(result, k=k)
  print(paste("Found ", k, "clusters"))
  
  # znalezienie środków wyznaczonych grup
  idx <- (clustering == 1)
  centroids <- colMeans(clusteringInput[idx,])
  for (i in 2:k) {
    idx <- (clustering == i)
    centroids <- rbind(centroids, colMeans(clusteringInput[idx,]))
  }
  print(paste("Found diana centroids"))
  
  distances <- dist(playersAttributesFinal, centroids, method="Manhattan")
  result.manhattan.centroids[[k-minK+1]] <- centroids
  
  # przypisanie wszystkich próbek do grup
  result.manhattan.clustering[[k-minK+1]] <- apply(distances, 1, which.min)
  print(paste("Found clusters for all examples"))
  
  # obliczenie metryk dla grupowania
  result.manhattan.metrics[[k-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering[[k-minK+1]])
  result.manhattan.compare.outfield[[k-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering[[k-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
  result.manhattan.compare.general[[k-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering[[k-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
  result.manhattan.compare.specific[[k-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering[[k-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
  
  print(paste("Evaluated metrics for", k, "clusters"))
  print("===========================================================================")
}

# usuwanie zbędnych macierzy
rm(manhattanDistanceMatrix)
rm(manhattanDistanceMatrixFull)

##########################################################################################

# macierze odległości dla pełnego i losowo dobranego, małego zbioru danych
minkowskiDistanceMatrix <- dist(clusteringInput, method="Minkowski", p=3)
minkowskiDistanceMatrixFull <- dist(playersAttributesFinal, method="Minkowski", p=3)

# alokacja tablic wynikowych
result.minkowski.centroids <- list()
result.minkowski.clustering <- list()
result.minkowski.metrics <- list()
result.minkowski.compare.outfield <- list()
result.minkowski.compare.general <- list()
result.minkowski.compare.specific <- list()

print("===========================================================================")
print("Minkowski")
print("===========================================================================")

# wywołanie funkcji realizującej grupowanie deglomeracyjne
result <- diana(minkowskiDistanceMatrix, diss=TRUE)

# pętla realizująca grupowanie dla różnych wartości k przy wykorzystaniu ogległości Minkowskiego
for (k in minK:maxK) {
  # znalezienie początkowego grupowania w zależności od k
  clustering <- cutree(result, k=k)
  print(paste("Found ", k, "clusters"))
  
  # znalezienie środków wyznaczonych grup
  idx <- (clustering == 1)
  centroids <- colMeans(clusteringInput[idx,])
  for (i in 2:k) {
    idx <- (clustering == i)
    centroids <- rbind(centroids, colMeans(clusteringInput[idx,]))
  }
  print(paste("Found diana centroids"))
  
  distances <- dist(playersAttributesFinal, centroids, method="Minkowski", p=3)
  result.minkowski.centroids[[k-minK+1]] <- centroids
  
  # przypisanie wszystkich próbek do grup
  result.minkowski.clustering[[k-minK+1]] <- apply(distances, 1, which.min)
  print(paste("Found clusters for all examples"))
  
  # obliczenie metryk dla grupowania
  result.minkowski.metrics[[k-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering[[k-minK+1]])
  result.minkowski.compare.outfield[[k-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering[[k-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
  result.minkowski.compare.general[[k-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering[[k-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
  result.minkowski.compare.specific[[k-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering[[k-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
  
  print(paste("Evaluated metrics for", k, "clusters"))
  print("===========================================================================")
}

# usuwanie zbędnych macierzy
rm(minkowskiDistanceMatrix)
rm(minkowskiDistanceMatrixFull)

##########################################################################################

# macierze odległości dla pełnego i losowo dobranego, małego zbioru danych
correlationDistanceMatrix <- dist(clusteringInput, method="correlation")
correlationDistanceMatrixFull <- dist(playersAttributesFinal, method="correlation")

# alokacja tablic wynikowych
result.correlation.centroids <- list()
result.correlation.clustering <- list()
result.correlation.metrics <- list()
result.correlation.compare.outfield <- list()
result.correlation.compare.general <- list()
result.correlation.compare.specific <- list()

print("===========================================================================")
print("Correlation")
print("===========================================================================")

# wywołanie funkcji realizującej grupowanie deglomeracyjne
result <- diana(correlationDistanceMatrix, diss=TRUE)

# petla realizujące grupowanie dla różnych wartości k przy wykorzystaniu ogległości korelacyjnej
for (k in minK:maxK) {
  # znalezienie początkowego grupowania w zależności od k
  clustering <- cutree(result, k=k)
  print(paste("Found ", k, "clusters"))
  
  # znalezienie środków wyznaczonych grup
  idx <- (clustering == 1)
  centroids <- colMeans(clusteringInput[idx,])
  for (i in 2:k) {
    idx <- (clustering == i)
    centroids <- rbind(centroids, colMeans(clusteringInput[idx,]))
  }
  print(paste("Found diana centroids"))
  
  distances <- dist(playersAttributesFinal, centroids, method="correlation")
  result.correlation.centroids[[k-minK+1]] <- centroids
  
  # przypisanie wszystkich próbek do grup
  result.correlation.clustering[[k-minK+1]] <- apply(distances, 1, which.min)
  print(paste("Found clusters for all examples"))
  
  # obliczenie metryk dla grupowania
  result.correlation.metrics[[k-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering[[k-minK+1]])
  result.correlation.compare.outfield[[k-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering[[k-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
  result.correlation.compare.general[[k-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering[[k-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
  result.correlation.compare.specific[[k-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering[[k-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
  
  print(paste("Evaluated metrics for", k, "clusters"))
  print("===========================================================================")
}

# usuwanie zbędnych macierzy
rm(correlationDistanceMatrix)
rm(correlationDistanceMatrixFull)

# zapis danych wynikowych w pliku .Rdata
save(result.euclidean.centroids,
     result.euclidean.clustering,
     result.euclidean.metrics,
     result.euclidean.compare.outfield,
     result.euclidean.compare.general,
     result.euclidean.compare.specific,
    
     result.manhattan.centroids,
     result.manhattan.clustering,
     result.manhattan.metrics,
     result.manhattan.compare.outfield,
     result.manhattan.compare.general,
     result.manhattan.compare.specific,
    
     result.minkowski.centroids,
     result.minkowski.clustering,
     result.minkowski.metrics,
     result.minkowski.compare.outfield,
     result.minkowski.compare.general,
     result.minkowski.compare.specific,
    
     result.correlation.centroids,
     result.correlation.clustering,
     result.correlation.metrics,
     result.correlation.compare.outfield,
     result.correlation.compare.general,
     result.correlation.compare.specific,
    
     clusteringInput, 
     file="../results/diana-results-shortened.RData"
)