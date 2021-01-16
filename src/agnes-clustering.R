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

#### range of numbers of clusters to create during tests
minK = 2
maxK = 40

#### linkage methods
linkageMethods <- c("average", "complete")

##########################################################################################

#### distance matrices for different methods
euclideanDistanceMatrix <- dist(clusteringInput, method="Euclidean")
euclideanDistanceMatrixFull <- dist(playersAttributesFinal, method="Euclidean")

# results
result.euclidean.centroids.average <- list()
result.euclidean.clustering.average <- list()
result.euclidean.metrics.average <- list()
result.euclidean.compare.outfield.average <- list()
result.euclidean.compare.general.average <- list()
result.euclidean.compare.specific.average <- list()

result.euclidean.centroids.complete <- list()
result.euclidean.clustering.complete <- list()
result.euclidean.metrics.complete <- list()
result.euclidean.compare.outfield.complete <- list()
result.euclidean.compare.general.complete <- list()
result.euclidean.compare.specific.complete <- list()

print("===========================================================================")
print("Euclidean")
print("===========================================================================")

for (m in 1:length(linkageMethods)) {
  print(paste("Method: ", linkageMethods[m]))
  print("===========================================================================")
  result <- agnes(euclideanDistanceMatrix, diss=TRUE, method=linkageMethods[m])
  for (k in minK:maxK) {
    # find initial clusters for sampled data.frame
    clustering <- cutree(result, k=k)
    print(paste("Found ", k, "clusters"))
    
    # find centroids of the clusters
    idx <- (clustering == 1)
    centroids <- colMeans(clusteringInput[idx,])
    for (i in 2:k) {
      idx <- (clustering == i)
      centroids <- rbind(centroids, colMeans(clusteringInput[idx,]))
    }
    print(paste("Found agnes centroids for ", linkageMethods[m], " and k: ", k))
    
    distances <- dist(playersAttributesFinal, centroids, method="Euclidean")
    
    if (linkageMethods[m] == 'average') {
      result.euclidean.centroids.average[[k-minK+1]] <- centroids
      
      # find clusters for all examples
      result.euclidean.clustering.average[[k-minK+1]] <- apply(distances, 1, which.min)
      print(paste("Found clusters for all examples, method: ", linkageMethods[m], " and k: ", k))
      
      # find metric values 
      result.euclidean.metrics.average[[k-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering.average[[k-minK+1]])
      result.euclidean.compare.outfield.average[[k-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering.average[[k-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
      result.euclidean.compare.general.average[[k-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering.average[[k-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
      result.euclidean.compare.specific.average[[k-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering.average[[k-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
      
      print(paste("Evaluated metrics for", k, "clusters, method: ", linkageMethods[m]))
      
    } else {
      result.euclidean.centroids.complete[[k-minK+1]] <- centroids
      
      # find clusters for all examples
      result.euclidean.clustering.complete[[k-minK+1]] <- apply(distances, 1, which.min)
      print(paste("Found clusters for all examples, method: ", linkageMethods[m], " and k: ", k))
      
      # find metric values 
      result.euclidean.metrics.complete[[k-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering.complete[[k-minK+1]])
      result.euclidean.compare.outfield.complete[[k-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering.complete[[k-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
      result.euclidean.compare.general.complete[[k-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering.complete[[k-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
      result.euclidean.compare.specific.complete[[k-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering.complete[[k-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
      
      print(paste("Evaluated metrics for", k, "clusters, method: ", linkageMethods[m]))
    }
    print("===========================================================================")
  }
}

rm(euclideanDistanceMatrix)
rm(euclideanDistanceMatrixFull)

save(
  result.euclidean.centroids.average,
  result.euclidean.clustering.average,
  result.euclidean.metrics.average,
  result.euclidean.compare.outfield.average,
  result.euclidean.compare.general.average,
  result.euclidean.compare.specific.average,
  
  result.euclidean.centroids.complete,
  result.euclidean.clustering.complete,
  result.euclidean.metrics.complete,
  result.euclidean.compare.outfield.complete,
  result.euclidean.compare.general.complete,
  result.euclidean.compare.specific.complete,
  
  clusteringInput, 
  file="../results/agnes-euclidean-results.RData"
)

##########################################################################################


manhattanDistanceMatrix <- dist(clusteringInput, method="Manhattan")
manhattanDistanceMatrixFull <- dist(playersAttributesFinal, method="Manhattan")

result.manhattan.centroids.average <- list()
result.manhattan.clustering.average <- list()
result.manhattan.metrics.average <- list()
result.manhattan.compare.outfield.average <- list()
result.manhattan.compare.general.average <- list()
result.manhattan.compare.specific.average <- list()

result.manhattan.centroids.complete <- list()
result.manhattan.clustering.complete <- list()
result.manhattan.metrics.complete <- list()
result.manhattan.compare.outfield.complete <- list()
result.manhattan.compare.general.complete <- list()
result.manhattan.compare.specific.complete <- list()

print("===========================================================================")
print("Manhattan")
print("===========================================================================")

for (m in 1:length(linkageMethods)) {
  print(paste("Method: ", linkageMethods[m]))
  print("===========================================================================")
  result <- agnes(manhattanDistanceMatrix, diss=TRUE, method=linkageMethods[m])
  for (k in minK:maxK) {
    # find initial clusters for sampled data.frame
    clustering <- cutree(result, k=k)
    print(paste("Found ", k, "clusters"))
    
    # find centroids of the clusters
    idx <- (clustering == 1)
    centroids <- colMeans(clusteringInput[idx,])
    for (i in 2:k) {
      idx <- (clustering == i)
      centroids <- rbind(centroids, colMeans(clusteringInput[idx,]))
    }
    print(paste("Found agnes centroids for ", linkageMethods[m], " and k: ", k))
    
    distances <- dist(playersAttributesFinal, centroids, method="Manhattan")
    
    if (linkageMethods[m] == 'average') {
      result.manhattan.centroids.average[[k-minK+1]] <- centroids
      
      # find clusters for all examples
      result.manhattan.clustering.average[[k-minK+1]] <- apply(distances, 1, which.min)
      print(paste("Found clusters for all examples, method: ", linkageMethods[m], " and k: ", k))
      
      # find metric values 
      result.manhattan.metrics.average[[k-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering.average[[k-minK+1]])
      result.manhattan.compare.outfield.average[[k-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering.average[[k-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
      result.manhattan.compare.general.average[[k-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering.average[[k-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
      result.manhattan.compare.specific.average[[k-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering.average[[k-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
      
      print(paste("Evaluated metrics for", k, "clusters, method: ", linkageMethods[m]))
      
    } else {
      result.manhattan.centroids.complete[[k-minK+1]] <- centroids
      
      # find clusters for all examples
      result.manhattan.clustering.complete[[k-minK+1]] <- apply(distances, 1, which.min)
      print(paste("Found clusters for all examples, method: ", linkageMethods[m], " and k: ", k))
      
      # find metric values 
      result.manhattan.metrics.complete[[k-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering.complete[[k-minK+1]])
      result.manhattan.compare.outfield.complete[[k-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering.complete[[k-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
      result.manhattan.compare.general.complete[[k-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering.complete[[k-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
      result.manhattan.compare.specific.complete[[k-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering.complete[[k-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
      
      print(paste("Evaluated metrics for", k, "clusters, method: ", linkageMethods[m]))
    }
    print("===========================================================================")
  }
}

rm(manhattanDistanceMatrix)
rm(manhattanDistanceMatrixFull)

save(
  result.manhattan.centroids.average,
  result.manhattan.clustering.average,
  result.manhattan.metrics.average,
  result.manhattan.compare.outfield.average,
  result.manhattan.compare.general.average,
  result.manhattan.compare.specific.average,
  
  result.manhattan.centroids.complete,
  result.manhattan.clustering.complete,
  result.manhattan.metrics.complete,
  result.manhattan.compare.outfield.complete,
  result.manhattan.compare.general.complete,
  result.manhattan.compare.specific.complete,
  
  clusteringInput, 
  file="../results/agnes-manhattan-results.RData"
)


##########################################################################################


minkowskiDistanceMatrix <- dist(clusteringInput, method="Minkowski", p=3)
minkowskiDistanceMatrixFull <- dist(playersAttributesFinal, method="Minkowski", p=3)

# results
result.minkowski.centroids.average <- list()
result.minkowski.clustering.average <- list()
result.minkowski.metrics.average <- list()
result.minkowski.compare.outfield.average <- list()
result.minkowski.compare.general.average <- list()
result.minkowski.compare.specific.average <- list()

result.minkowski.centroids.complete <- list()
result.minkowski.clustering.complete <- list()
result.minkowski.metrics.complete <- list()
result.minkowski.compare.outfield.complete <- list()
result.minkowski.compare.general.complete <- list()
result.minkowski.compare.specific.complete <- list()

print("===========================================================================")
print("Minkowski")
print("===========================================================================")

for (m in 1:length(linkageMethods)) {
  print(paste("Method: ", linkageMethods[m]))
  print("===========================================================================")
  result <- agnes(minkowskiDistanceMatrix, diss=TRUE, method=linkageMethods[m])
  for (k in minK:maxK) {
    # find initial clusters for sampled data.frame
    clustering <- cutree(result, k=k)
    print(paste("Found ", k, "clusters"))
    
    # find centroids of the clusters
    idx <- (clustering == 1)
    centroids <- colMeans(clusteringInput[idx,])
    for (i in 2:k) {
      idx <- (clustering == i)
      centroids <- rbind(centroids, colMeans(clusteringInput[idx,]))
    }
    print(paste("Found agnes centroids for ", linkageMethods[m], " and k: ", k))
    
    distances <- dist(playersAttributesFinal, centroids, method="Minkowski", p=3)
    
    if (linkageMethods[m] == 'average') {
      result.minkowski.centroids.average[[k-minK+1]] <- centroids
      
      # find clusters for all examples
      result.minkowski.clustering.average[[k-minK+1]] <- apply(distances, 1, which.min)
      print(paste("Found clusters for all examples, method: ", linkageMethods[m], " and k: ", k))
      
      # find metric values 
      result.minkowski.metrics.average[[k-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering.average[[k-minK+1]])
      result.minkowski.compare.outfield.average[[k-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering.average[[k-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
      result.minkowski.compare.general.average[[k-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering.average[[k-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
      result.minkowski.compare.specific.average[[k-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering.average[[k-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
      
      print(paste("Evaluated metrics for", k, "clusters, method: ", linkageMethods[m]))
      
    } else {
      result.minkowski.centroids.complete[[k-minK+1]] <- centroids
      
      # find clusters for all examples
      result.minkowski.clustering.complete[[k-minK+1]] <- apply(distances, 1, which.min)
      print(paste("Found clusters for all examples, method: ", linkageMethods[m], " and k: ", k))
      
      # find metric values 
      result.minkowski.metrics.complete[[k-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering.complete[[k-minK+1]])
      result.minkowski.compare.outfield.complete[[k-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering.complete[[k-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
      result.minkowski.compare.general.complete[[k-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering.complete[[k-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
      result.minkowski.compare.specific.complete[[k-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering.complete[[k-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
      
      print(paste("Evaluated metrics for", k, "clusters, method: ", linkageMethods[m]))
    }
    print("===========================================================================")
  }
}

rm(minkowskiDistanceMatrix)
rm(minkowskiDistanceMatrixFull)

save(
  result.minkowski.centroids.average,
  result.minkowski.clustering.average,
  result.minkowski.metrics.average,
  result.minkowski.compare.outfield.average,
  result.minkowski.compare.general.average,
  result.minkowski.compare.specific.average,
  
  result.minkowski.centroids.complete,
  result.minkowski.clustering.complete,
  result.minkowski.metrics.complete,
  result.minkowski.compare.outfield.complete,
  result.minkowski.compare.general.complete,
  result.minkowski.compare.specific.complete,
  
  clusteringInput, 
  file="../results/agnes-minkowski-results.RData"
)

##########################################################################################


correlationDistanceMatrix <- dist(clusteringInput, method="correlation")
correlationDistanceMatrixFull <- dist(playersAttributesFinal, method="correlation")

# results
result.correlation.centroids.average <- list()
result.correlation.clustering.average <- list()
result.correlation.metrics.average <- list()
result.correlation.compare.outfield.average <- list()
result.correlation.compare.general.average <- list()
result.correlation.compare.specific.average <- list()

result.correlation.centroids.complete <- list()
result.correlation.clustering.complete <- list()
result.correlation.metrics.complete <- list()
result.correlation.compare.outfield.complete <- list()
result.correlation.compare.general.complete <- list()
result.correlation.compare.specific.complete <- list()

print("===========================================================================")
print("Correlation")
print("===========================================================================")

for (m in 1:length(linkageMethods)) {
  print(paste("Method: ", linkageMethods[m]))
  print("===========================================================================")
  result <- agnes(correlationDistanceMatrix, diss=TRUE, method=linkageMethods[m])
  for (k in minK:maxK) {
    # find initial clusters for sampled data.frame
    clustering <- cutree(result, k=k)
    print(paste("Found ", k, "clusters"))
    
    # find centroids of the clusters
    idx <- (clustering == 1)
    centroids <- colMeans(clusteringInput[idx,])
    for (i in 2:k) {
      idx <- (clustering == i)
      centroids <- rbind(centroids, colMeans(clusteringInput[idx,]))
    }
    print(paste("Found agnes centroids for ", linkageMethods[m], " and k: ", k))
    
    distances <- dist(playersAttributesFinal, centroids, method="correlation")
    
    if (linkageMethods[m] == 'average') {
      result.correlation.centroids.average[[k-minK+1]] <- centroids
      
      # find clusters for all examples
      result.correlation.clustering.average[[k-minK+1]] <- apply(distances, 1, which.min)
      print(paste("Found clusters for all examples, method: ", linkageMethods[m], " and k: ", k))
      
      # find metric values 
      result.correlation.metrics.average[[k-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering.average[[k-minK+1]])
      result.correlation.compare.outfield.average[[k-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering.average[[k-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
      result.correlation.compare.general.average[[k-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering.average[[k-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
      result.correlation.compare.specific.average[[k-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering.average[[k-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
      
      print(paste("Evaluated metrics for", k, "clusters, method: ", linkageMethods[m]))
      
    } else {
      result.correlation.centroids.complete[[k-minK+1]] <- centroids
      
      # find clusters for all examples
      result.correlation.clustering.complete[[k-minK+1]] <- apply(distances, 1, which.min)
      print(paste("Found clusters for all examples, method: ", linkageMethods[m], " and k: ", k))
      
      # find metric values 
      result.correlation.metrics.complete[[k-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering.complete[[k-minK+1]])
      result.correlation.compare.outfield.complete[[k-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering.complete[[k-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
      result.correlation.compare.general.complete[[k-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering.complete[[k-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
      result.correlation.compare.specific.complete[[k-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering.complete[[k-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
      
      print(paste("Evaluated metrics for", k, "clusters, method: ", linkageMethods[m]))
    }
    print("===========================================================================")
  }
}

rm(correlationDistanceMatrix)
rm(correlationDistanceMatrixFull)

save(
  result.correlation.centroids.average,
  result.correlation.clustering.average,
  result.correlation.metrics.average,
  result.correlation.compare.outfield.average,
  result.correlation.compare.general.average,
  result.correlation.compare.specific.average,
  
  result.correlation.centroids.complete,
  result.correlation.clustering.complete,
  result.correlation.metrics.complete,
  result.correlation.compare.outfield.complete,
  result.correlation.compare.general.complete,
  result.correlation.compare.specific.complete,
  
  clusteringInput, 
  file="../results/agnes-correlation-results.RData"
)