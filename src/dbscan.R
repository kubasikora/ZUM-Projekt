# Skrypt realizujący grupowanie gęstościowe dla różnych wartości promienia sąsiedztwa i minimalnej liczby sąsiadów. Wynikiem wykonania
# stryptu jest plik zawierający dane zawierające wartości miar jakości grupowania do dalszej analizy.

library(dbscan)
library(fpc)

# wczytywanie danych
source("./data-preparation.R")
source("./smaller-attributes-prep.R")

# wybór losowych danych o zdefiniowanym rozmiarze
SAMPLE_RATIO <- 0.1
clusteringInput <- slice_sample(playersAttributesFinal, prop=SAMPLE_RATIO)
summary(clusteringInput)

# macierz odległości wyznaczana przy pomocy odległości korelacyjnej
correlationDistanceMatrix <- dist(clusteringInput, method="correlation")

# alokacja tablic wynikowych
cl_dbscan <- list()
pts <- list()
epsilons <- list()
result.dbscan.metrics <- list()
result.dbscan.compare.outfield <- list()
result.dbscan.compare.general <- list()
result.dbscan.compare.specific <- list()

counter = 1

# główna pętla eksperymentu
for(p in c(5, 10, 15, 20, 25)){
  for(epsilon in c(0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875)){
  print(c(p, epsilon))

  # przeprowadzenie grupowania
  cl <- fpc::dbscan(correlationDistanceMatrix, MinPts=p, eps=epsilon)
  cl$cluster <- replace(cl$cluster, cl$cluster == 0, 10000)
  cl_dbscan[[counter]] <- cl
  pts[[counter]] <- p
  epsilons[[counter]] <- epsilon
  
  print("stats")
  if(length(unique(cl$cluster)) == 1){
    print("no clusters detected")
    result.dbscan.metrics[[counter]] <- NULL
  } else {
    result.dbscan.metrics[[counter]] <- cluster.stats(correlationDistanceMatrix, noisecluster = TRUE, cl$cluster)
  }
  counter <- counter + 1
  }
}

# zapis danych wynikowych w pliku .Rdata
save(cl_dbscan, result.dbscan.metrics, file="./dbscan-results.RData")
