# Skrypt realizujący eksperymenty wykorzystujące algorytm k-medoids. Eksperymenty przeprowadzane  
# za pomocą skryptu pozwalają na uzyskanie danych opisujących wpływ wykorzystywanych miar niepodobieństwa
# na jakość grupowania. Wynikiem skryptu jest plik zawierający dane zawierające wartości miar jakości
# grupowania do dalszej analizy.

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

# algorytm k-medoids dla odległości euklidesowej
print("Euclidean")

# macierze odległości dla pełnego i losowo dobranego małego zbioru danych
euclideanDistanceMatrix <- dist(clusteringInput, method="Euclidean")
euclideanDistanceMatrixFull <- dist(playersAttributesFinal, method="Euclidean")

# alokacja tablic wynikowych
result.euclidean.pams <- list()
result.euclidean.centroids <- list()
result.euclidean.clustering <- list()
result.euclidean.metrics <- list()
result.euclidean.compare.outfield <- list()
result.euclidean.compare.general <- list()
result.euclidean.compare.specific <- list()

# pętla, w której wykonywany jest algorytm pam dla odległości euklidesowej
# oraz zbierane są wyniki grupowania
for(i in minK:maxK){
    # znalezienie środków grup dla mniejszego zbioru danych
    result <- pam(euclideanDistanceMatrix, i, diss=TRUE, pamonce=5, keep.diss=TRUE)
    print(paste("Found ", i, "clusters"))
    
    result.euclidean.pams[[i-minK+1]] <- result
    result.euclidean.centroids[[i-minK+1]] <- result$medoids
    
    # znalezienie klastrów dla wszystkich próbek
    medoids <- playersAttributesFinal[result$medoids,]
    distances <- dist(playersAttributesFinal, medoids, method="Euclidean")
    result.euclidean.clustering[[i-minK+1]] <- apply(distances, 1, which.min)
    print(paste("Found clusters for all examples"))
    
    # obliczenie metryk
    result.euclidean.metrics[[i-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering[[i-minK+1]])
    result.euclidean.compare.outfield[[i-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering[[i-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
    result.euclidean.compare.general[[i-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering[[i-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
    result.euclidean.compare.specific[[i-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering[[i-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
    print(paste("Evaluated metrics for", i, "clusters"))
}

# usuwanie zbędnych macierzy
rm(euclideanDistanceMatrix)
rm(euclideanDistanceMatrixFull)

# algorytm k-medioids z odległością minkowskiego distance z parametrem p = 3
print("Minkowski")

# macierze odległości dla pełnego i losowo dobranego małego zbioru danych
minkowskiDistanceMatrix <- dist(clusteringInput, method="Minkowski", p=3)
minkowskiDistanceMatrixFull <- dist(playersAttributesFinal, method="Minkowski", p=3)

# alokacja tablic wynikowych
result.minkowski.pams <- list()
result.minkowski.centroids <- list()
result.minkowski.clustering <- list()
result.minkowski.metrics <- list()
result.minkowski.compare.outfield <- list()
result.minkowski.compare.general <- list()
result.minkowski.compare.specific <- list()

# pętla, w której wykonywany jest algorytm pam dla odległości minkowskiego
# oraz zbierane są wyniki grupowania
for(i in minK:maxK){
    # znalezienie środków grup dla mniejszego zbioru danych
    result <- pam(minkowskiDistanceMatrix, i, diss=TRUE, pamonce=5, keep.diss=TRUE)
    print(paste("Found ", i, "clusters"))
    
    result.minkowski.pams[[i-minK+1]] <- result
    result.minkowski.centroids[[i-minK+1]] <- result$medoids
    
    # znalezienie klastrów dla wszystkich próbek
    medoids <- playersAttributesFinal[result$medoids,]
    distances <- dist(playersAttributesFinal, medoids, method="Minkowski", p=3)
    result.minkowski.clustering[[i-minK+1]] <- apply(distances, 1, which.min)
    print(paste("Found clusters for all examples"))
    
    # obliczenie metryk
    result.minkowski.metrics[[i-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering[[i-minK+1]])
    result.minkowski.compare.outfield[[i-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering[[i-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
    result.minkowski.compare.general[[i-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering[[i-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
    result.minkowski.compare.specific[[i-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering[[i-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
    print(paste("Evaluated metrics for", i, "clusters"))
}

# usuwanie zbędnych macierzy
rm(minkowskiDistanceMatrix)
rm(minkowskiDistanceMatrixFull)

#### algorytm k-medioids dla odległości manhattan 
print("Manhattan")

# macierze odległości dla pełnego i losowo dobranego małego zbioru danych
manhattanDistanceMatrix <- dist(clusteringInput, method="Manhattan")
manhattanDistanceMatrixFull <- dist(playersAttributesFinal, method="Manhattan")

# alokacja tablic wynikowych
result.manhattan.pams <- list()
result.manhattan.centroids <- list()
result.manhattan.clustering <- list()
result.manhattan.metrics <- list()
result.manhattan.compare.outfield <- list()
result.manhattan.compare.general <- list()
result.manhattan.compare.specific <- list()

# pętla, w której wykonywany jest algorytm pam dla odległości Manhattan
# oraz zbierane są wyniki grupowania
for(i in minK:maxK){
    # znalezienie środków grup dla mniejszego zbioru danych
    result <- pam(manhattanDistanceMatrix, i, diss=TRUE, pamonce=5, keep.diss=TRUE)
    print(paste("Found ", i, "clusters"))
    
    result.manhattan.pams[[i-minK+1]] <- result
    result.manhattan.centroids[[i-minK+1]] <- result$medoids
    
    # znalezienie klastrów dla wszystkich próbek
    medoids <- playersAttributesFinal[result$medoids,]
    distances <- dist(playersAttributesFinal, medoids, method="Manhattan")
    result.manhattan.clustering[[i-minK+1]] <- apply(distances, 1, which.min)
    print(paste("Found clusters for all examples"))
    
    # obliczenie metryk
    result.manhattan.metrics[[i-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering[[i-minK+1]])
    result.manhattan.compare.outfield[[i-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering[[i-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
    result.manhattan.compare.general[[i-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering[[i-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
    result.manhattan.compare.specific[[i-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering[[i-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
    print(paste("Evaluated metrics for", i, "clusters"))
}

# usuwanie zbędnych macierzy
rm(manhattanDistanceMatrix)
rm(manhattanDistanceMatrixFull)

# k-medioids z odległością korelacyjną
print("Correlation")

# macierze odległości dla pełnego i losowo dobranego małego zbioru danych
correlationDistanceMatrix <- dist(clusteringInput, method="correlation")
correlationDistanceMatrixFull <- dist(playersAttributesFinal, method="correlation")

# alokacja tablic wynikowych
result.correlation.pams <- list()
result.correlation.centroids <- list()
result.correlation.clustering <- list()
result.correlation.metrics <- list()
result.correlation.compare.outfield <- list()
result.correlation.compare.general <- list()
result.correlation.compare.specific <- list()

# pętla, w której wykonywany jest algorytm pam z odległością korelacyjną
# oraz zbierane są wyniki grupowania
for(i in minK:maxK){
    # znalezienie środków grup dla mniejszego zbioru danych
    result <- pam(euclideanDistanceMatrix, i, diss=TRUE, pamonce=5, keep.diss=TRUE)
    print(paste("Found ", i, "clusters"))
    
    result.correlation.pams[[i-minK+1]] <- result
    result.correlation.centroids[[i-minK+1]] <- result$medoids
    
    # znalezienie klastrów dla wszystkich próbek
    medoids <- playersAttributesFinal[result$medoids,]
    distances <- dist(playersAttributesFinal, medoids, method="correlation")
    result.correlation.clustering[[i-minK+1]] <- apply(distances, 1, which.min)
    print(paste("Found clusters for all examples"))
    
    # obliczenie metryk
    result.correlation.metrics[[i-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering[[i-minK+1]])
    result.correlation.compare.outfield[[i-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering[[i-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
    result.correlation.compare.general[[i-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering[[i-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
    result.correlation.compare.specific[[i-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering[[i-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
    print(paste("Evaluated metrics for", i, "clusters"))
}

# usuwanie zbędnych macierzy
rm(correlationDistanceMatrix)
rm(correlationDistanceMatrixFull)

# zapis danych wynikowych w pliku .Rdata
save(result.correlation.pams, 
     result.correlation.centroids, 
     result.correlation.clustering,
     result.correlation.metrics,
     result.correlation.compare.outfield,
     result.correlation.compare.general,
     result.correlation.compare.specific,
     
     result.manhattan.pams, 
     result.manhattan.centroids, 
     result.manhattan.clustering, 
     result.manhattan.metrics,
     result.manhattan.compare.outfield,
     result.manhattan.compare.general,
     result.manhattan.compare.specific,
     
     result.minkowski.pams, 
     result.minkowski.clustering, 
     result.minkowski.centroids,
     result.minkowski.metrics,
     result.minkowski.compare.outfield,
     result.minkowski.compare.general,
     result.minkowski.compare.specific,
     
     result.euclidean.pams, 
     result.euclidean.clustering, 
     result.euclidean.centroids, 
     result.euclidean.metrics,
     result.euclidean.compare.outfield,
     result.euclidean.compare.general,
     result.euclidean.compare.specific,
     
     clusteringInput, 
     file="./results/kmedoids-results-full.RData"
)
