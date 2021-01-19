library(dbscan)

source("./dataPreparation.R")

# use euclidean metric for calculating distances 
euclideanDistanceMatrix <- dist(playersAttributesFinal, method="Euclidean")

# knee method
dbscan::kNNdistplot(euclideanDistanceMatrix, k=10)
abline(h=3.5, lty="dashed")

# conduct clustering
cl_dbscan <- dbscan(euclideanDistanceMatrix, minPts=10, eps=3.5)

result.dbscan.metrics <- cluster.stats(euclideanDistanceMatrix, cl_dbscan$cluster)
result.dbscan.compare.outfield <- cluster.stats(euclideanDistanceMatrix, cl_dbscan$cluster, alt.clustering=outfieldTrueClustering, compareonly=TRUE)
result.dbscan.compare.general <- cluster.stats(euclideanDistanceMatrix, cl_dbscan$cluster, alt.clustering=generalTrueClustering, compareonly=TRUE)
result.dbscan.compare.specific <- cluster.stats(euclideanDistanceMatrix, cl_dbscan$cluster, alt.clustering=specificTrueClustering, compareonly=TRUE)

result.dbscan.metrics$avg.silwidth

result.dbscan.metrics$within.cluster.ss

result.dbscan.compare.outfield$corrected.rand
result.dbscan.compare.general$corrected.rand
result.dbscan.compare.specific$corrected.rand

save(
  cl_dbscan,
  result.dbscan.metrics,
  result.dbscan.compare.outfield,
  result.dbscan.compare.general,
  result.dbscan.compare.specific, 
  file="../results/dbscan-results.RData"
)