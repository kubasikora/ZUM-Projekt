library(dbscan)
library(fpc)
source("./data-preparation.R")
source("./smaller-attributes-prep.R")

SAMPLE_RATIO <- 0.1
clusteringInput <- slice_sample(playersAttributesFinal, prop=SAMPLE_RATIO)
summary(clusteringInput)

correlationDistanceMatrix <- dist(clusteringInput, method="correlation")

# knee method

cl_dbscan <- list()
pts <- list()
epsilons <- list()
result.dbscan.metrics <- list()
result.dbscan.compare.outfield <- list()
result.dbscan.compare.general <- list()
result.dbscan.compare.specific <- list()

counter = 1
# conduct clustering
for(p in c(5, 10, 15, 20, 25)){
  for(epsilon in c(0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875)){
  print(c(p, epsilon))
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

save(cl_dbscan, result.dbscan.metrics, file="./dbscan-results.RData")
