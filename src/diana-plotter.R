library(mltools)
library(cluster)
library(dplyr)
library(proxy)
library(clv)
library(clusterCrit)
library(fpc)

load("../results/diana-results.RData")

minK <- 2
maxK <- 40

clustersK<- list()
cluster.ss.euclidean <- list()
cluster.ss.manhattan <- list()
cluster.ss.minkowski <- list()
cluster.ss.correlation <- list()

avg.sil.widths.euclidean <- list()
avg.sil.widths.manhattan <- list()
avg.sil.widths.minkowski <- list()
avg.sil.widths.correlation <- list()

chs.euclidean <- list()
chs.manhattan <- list()
chs.minkowski <- list()
chs.correlation <- list()

dunns.euclidean <- list()
dunns.manhattan <- list()
dunns.minkowski <- list()
dunns.correlation <- list()

rand.general <- list()
rand.outfield <- list()
rand.specific <- list()
vi.general <- list()
vi.outfield <- list()
vi.specific <- list()

for(i in minK:maxK){
  clustersK[[i-minK+1]] <- i
  # unsupervised
  cluster.ss.euclidean[[i-minK+1]] <- result.euclidean.metrics[[i-minK+1]]$within.cluster.ss
  cluster.ss.manhattan[[i-minK+1]] <- result.manhattan.metrics[[i-minK+1]]$within.cluster.ss
  cluster.ss.minkowski[[i-minK+1]] <- result.minkowski.metrics[[i-minK+1]]$within.cluster.ss
  cluster.ss.correlation[[i-minK+1]] <- result.correlation.metrics[[i-minK+1]]$within.cluster.ss
  
  avg.sil.widths.euclidean[[i-minK+1]] <- result.euclidean.metrics[[i-minK+1]]$avg.silwidth
  avg.sil.widths.manhattan[[i-minK+1]] <- result.manhattan.metrics[[i-minK+1]]$avg.silwidth
  avg.sil.widths.minkowski[[i-minK+1]] <- result.minkowski.metrics[[i-minK+1]]$avg.silwidth
  avg.sil.widths.correlation[[i-minK+1]] <- result.correlation.metrics[[i-minK+1]]$avg.silwidth
  
  chs.euclidean[[i-minK+1]] <- result.euclidean.metrics[[i-minK+1]]$ch
  chs.manhattan[[i-minK+1]] <- result.manhattan.metrics[[i-minK+1]]$ch
  chs.minkowski[[i-minK+1]] <- result.minkowski.metrics[[i-minK+1]]$ch
  chs.correlation[[i-minK+1]] <- result.correlation.metrics[[i-minK+1]]$ch
  
  dunns.euclidean[[i-minK+1]] <- result.euclidean.metrics[[i-minK+1]]$dunn
  dunns.manhattan[[i-minK+1]] <- result.manhattan.metrics[[i-minK+1]]$dunn
  dunns.minkowski[[i-minK+1]] <- result.minkowski.metrics[[i-minK+1]]$dunn
  dunns.correlation[[i-minK+1]] <- result.correlation.metrics[[i-minK+1]]$dunn
  
  # supervised
  rand.general[[i-minK+1]] <- result.euclidean.compare.general[[i-minK+1]]$corrected.rand
  rand.outfield[[i-minK+1]] <- result.euclidean.compare.outfield[[i-minK+1]]$corrected.rand
  rand.specific[[i-minK+1]] <- result.euclidean.compare.specific[[i-minK+1]]$corrected.rand
  
  vi.general[[i-minK+1]] <- result.euclidean.compare.general[[i-minK+1]]$vi
  vi.outfield[[i-minK+1]] <- result.euclidean.compare.outfield[[i-minK+1]]$vi
  vi.specific[[i-minK+1]] <- result.euclidean.compare.specific[[i-minK+1]]$vi
}

plot(clustersK, cluster.ss.euclidean, col="red", type="l")
plot(clustersK, cluster.ss.manhattan, col="blue", type="l")
plot(clustersK, cluster.ss.minkowski, col="green", type="l")
plot(clustersK, cluster.ss.correlation, col="black", type="l")

plot(clustersK, avg.sil.widths.euclidean, col="red", ylim=c(0.0, 0.5), type="l")
points(clustersK, avg.sil.widths.manhattan, col="blue", type="l")
points(clustersK, avg.sil.widths.minkowski, col="green", type="l")
points(clustersK, avg.sil.widths.correlation, col="black", type="l")

plot(clustersK, chs.euclidean, col="red", ylim=c(0.0, 8000), type="l")
points(clustersK, chs.manhattan, col="blue", type="l")
points(clustersK, chs.minkowski, col="green", type="l")
points(clustersK, chs.correlation, col="black", type="l")

plot(clustersK, dunns.euclidean, col="red", ylim=c(0.0, 0.08), type="l")
points(clustersK, dunns.manhattan, col="blue", type="l")
points(clustersK, dunns.minkowski, col="green", type="l")
points(clustersK, dunns.correlation, col="black", type="l")

plot(clustersK, rand.general, ylim=c(0.0, 1.0), type="l", col="red")
lines(clustersK, rand.outfield, col="blue")
lines(clustersK, rand.specific, col="green")

plot(clustersK, vi.general, ylim=c(0.0, 5.0), type="l", col="red")
lines(clustersK, vi.outfield, col="blue")
lines(clustersK, vi.specific, col="green")
