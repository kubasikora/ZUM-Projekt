library(mltools)
library(cluster)
library(dplyr)
library(proxy)
library(clv)
library(clusterCrit)
library(fpc)

source("./dataPreparation.R")

#### select random sample from the data
SAMPLE_RATIO <- 0.1
clusteringInput <- slice_sample(playersAttributesFinal, prop=SAMPLE_RATIO)
summary(clusteringInput)

#### pam version -> k-medioids with euclidean distance
minK = 2
maxK = 40

print("Euclidean")
euclideanDistanceMatrix <- dist(clusteringInput, method="Euclidean")
euclideanDistanceMatrixFull <- dist(playersAttributesFinal, method="Euclidean")
result.euclidean.pams <- list()
result.euclidean.centroids <- list()
result.euclidean.clustering <- list()
result.euclidean.metrics <- list()
result.euclidean.compare.outfield <- list()
result.euclidean.compare.general <- list()
result.euclidean.compare.specific <- list()

for(i in minK:maxK){
    # find initial clusters for sampled data.frame
    result <- pam(euclideanDistanceMatrix, i, diss=TRUE, pamonce=5, keep.diss=TRUE)
    print(paste("Found ", i, "clusters"))
    
    result.euclidean.pams[[i-minK+1]] <- result
    result.euclidean.centroids[[i-minK+1]] <- result$medoids
    
    # find clusters for all examples
    medoids <- playersAttributesFinal[result$medoids,]
    distances <- dist(playersAttributesFinal, medoids, method="Euclidean")
    result.euclidean.clustering[[i-minK+1]] <- apply(distances, 1, which.min)
    print(paste("Found clusters for all examples"))
    
    # find metric values 
    result.euclidean.metrics[[i-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering[[i-minK+1]])
    result.euclidean.compare.outfield[[i-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering[[i-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
    result.euclidean.compare.general[[i-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering[[i-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
    result.euclidean.compare.specific[[i-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering[[i-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
    print(paste("Evaluated metrics for", i, "clusters"))
}
rm(euclideanDistanceMatrix)
rm(euclideanDistanceMatrixFull)

### pam version -> k-medioids with minkowski distance with p = 3

print("Minkowski")
minkowskiDistanceMatrix <- dist(clusteringInput, method="Minkowski", p=3)
minkowskiDistanceMatrixFull <- dist(playersAttributesFinal, method="Minkowski", p=3)
result.minkowski.pams <- list()
result.minkowski.centroids <- list()
result.minkowski.clustering <- list()
result.minkowski.metrics <- list()
result.minkowski.compare.outfield <- list()
result.minkowski.compare.general <- list()
result.minkowski.compare.specific <- list()

for(i in minK:maxK){
    # find initial clusters for sampled data.frame
    result <- pam(minkowskiDistanceMatrix, i, diss=TRUE, pamonce=5, keep.diss=TRUE)
    print(paste("Found ", i, "clusters"))
    
    result.minkowski.pams[[i-minK+1]] <- result
    result.minkowski.centroids[[i-minK+1]] <- result$medoids
    
    # find clusters for all examples
    medoids <- playersAttributesFinal[result$medoids,]
    distances <- dist(playersAttributesFinal, medoids, method="Minkowski", p=3)
    result.minkowski.clustering[[i-minK+1]] <- apply(distances, 1, which.min)
    print(paste("Found clusters for all examples"))
    
    # find metric values 
    result.minkowski.metrics[[i-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering[[i-minK+1]])
    result.minkowski.compare.outfield[[i-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering[[i-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
    result.minkowski.compare.general[[i-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering[[i-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
    result.minkowski.compare.specific[[i-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering[[i-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
    print(paste("Evaluated metrics for", i, "clusters"))
}
rm(minkowskiDistanceMatrix)
rm(minkowskiDistanceMatrixFull)

#### pam version -> k-medioids with manhattan distance

print("Manhattan")
manhattanDistanceMatrix <- dist(clusteringInput, method="Manhattan")
manhattanDistanceMatrixFull <- dist(playersAttributesFinal, method="Manhattan")
result.manhattan.pams <- list()
result.manhattan.centroids <- list()
result.manhattan.clustering <- list()
result.manhattan.metrics <- list()
result.manhattan.compare.outfield <- list()
result.manhattan.compare.general <- list()
result.manhattan.compare.specific <- list()

for(i in minK:maxK){
    # find initial clusters for sampled data.frame
    result <- pam(manhattanDistanceMatrix, i, diss=TRUE, pamonce=5, keep.diss=TRUE)
    print(paste("Found ", i, "clusters"))
    
    result.manhattan.pams[[i-minK+1]] <- result
    result.manhattan.centroids[[i-minK+1]] <- result$medoids
    
    # find clusters for all examples
    medoids <- playersAttributesFinal[result$medoids,]
    distances <- dist(playersAttributesFinal, medoids, method="Manhattan")
    result.manhattan.clustering[[i-minK+1]] <- apply(distances, 1, which.min)
    print(paste("Found clusters for all examples"))
    
    # find metric values 
    result.manhattan.metrics[[i-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering[[i-minK+1]])
    result.manhattan.compare.outfield[[i-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering[[i-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
    result.manhattan.compare.general[[i-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering[[i-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
    result.manhattan.compare.specific[[i-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering[[i-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
    print(paste("Evaluated metrics for", i, "clusters"))
}
rm(manhattanDistanceMatrix)
rm(manhattanDistanceMatrixFull)

#### pam version -> k-medioids with correlation as distance

print("Correlation")
correlationDistanceMatrix <- dist(clusteringInput, method="correlation")
correlationDistanceMatrixFull <- dist(playersAttributesFinal, method="correlation")
result.correlation.pams <- list()
result.correlation.centroids <- list()
result.correlation.clustering <- list()
result.correlation.metrics <- list()
result.correlation.compare.outfield <- list()
result.correlation.compare.general <- list()
result.correlation.compare.specific <- list()

for(i in minK:maxK){
    # find initial clusters for sampled data.frame
    result <- pam(euclideanDistanceMatrix, i, diss=TRUE, pamonce=5, keep.diss=TRUE)
    print(paste("Found ", i, "clusters"))
    
    result.correlation.pams[[i-minK+1]] <- result
    result.correlation.centroids[[i-minK+1]] <- result$medoids
    
    # find clusters for all examples
    medoids <- playersAttributesFinal[result$medoids,]
    distances <- dist(playersAttributesFinal, medoids, method="correlation")
    result.correlation.clustering[[i-minK+1]] <- apply(distances, 1, which.min)
    print(paste("Found clusters for all examples"))
    
    # find metric values 
    result.correlation.metrics[[i-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering[[i-minK+1]])
    result.correlation.compare.outfield[[i-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering[[i-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
    result.correlation.compare.general[[i-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering[[i-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
    result.correlation.compare.specific[[i-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering[[i-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
    print(paste("Evaluated metrics for", i, "clusters"))
}
rm(correlationDistanceMatrix)
rm(correlationDistanceMatrixFull)

#### store results in Rdata file for further analysis

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
