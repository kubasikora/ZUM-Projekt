library(mltools)
library(cluster)
library(dplyr)
library(proxy)
library(clv)
library(clusterCrit)
library(fpc)

load("../results/agnes-euclidean-results.RData")
load("../results/agnes-minkowski-results.RData")
load("../results/agnes-manhattan-results.RData")
load("../results/agnes-correlation-results.RData")

minK <- 2
maxK <- 40

clustersK <- list()

cluster.ss.euclidean.average <- list()
cluster.ss.manhattan.average <- list()
cluster.ss.minkowski.average <- list()
cluster.ss.correlation.average <- list()
cluster.ss.euclidean.complete<- list()
cluster.ss.manhattan.complete<- list()
cluster.ss.minkowski.complete<- list()
cluster.ss.correlation.complete<- list()

avg.sil.widths.euclidean.average <- list()
avg.sil.widths.manhattan.average <- list()
avg.sil.widths.minkowski.average <- list()
avg.sil.widths.correlation.average <- list()
avg.sil.widths.euclidean.complete<- list()
avg.sil.widths.manhattan.complete<- list()
avg.sil.widths.minkowski.complete<- list()
avg.sil.widths.correlation.complete<- list()

chs.euclidean.average <- list()
chs.manhattan.average <- list()
chs.minkowski.average <- list()
chs.correlation.average <- list()
chs.euclidean.complete<- list()
chs.manhattan.complete<- list()
chs.minkowski.complete<- list()
chs.correlation.complete<- list()

dunns.euclidean.average <- list()
dunns.manhattan.average <- list()
dunns.minkowski.average <- list()
dunns.correlation.average <- list()
dunns.euclidean.complete<- list()
dunns.manhattan.complete<- list()
dunns.minkowski.complete<- list()
dunns.correlation.complete<- list()

rand.general.average <- list()
rand.outfield.average <- list()
rand.specific.average <- list()
rand.general.complete <- list()
rand.outfield.complete <- list()
rand.specific.complete <- list()

vi.general.average <- list()
vi.outfield.average <- list()
vi.specific.average <- list()
vi.general.complete <- list()
vi.outfield.complete <- list()
vi.specific.complete <- list()

for(i in minK:maxK){
  clustersK[[i-minK+1]] <- i
  #### unsupervised
  # average linkage
  cluster.ss.euclidean.average[[i-minK+1]] <- result.euclidean.metrics.average[[i-minK+1]]$within.cluster.ss
  cluster.ss.manhattan.average[[i-minK+1]] <- result.manhattan.metrics.average[[i-minK+1]]$within.cluster.ss
  cluster.ss.minkowski.average[[i-minK+1]] <- result.minkowski.metrics.average[[i-minK+1]]$within.cluster.ss
  cluster.ss.correlation.average[[i-minK+1]] <- result.correlation.metrics.average[[i-minK+1]]$within.cluster.ss
  
  avg.sil.widths.euclidean.average[[i-minK+1]] <- result.euclidean.metrics.average[[i-minK+1]]$avg.silwidth
  avg.sil.widths.manhattan.average[[i-minK+1]] <- result.manhattan.metrics.average[[i-minK+1]]$avg.silwidth
  avg.sil.widths.minkowski.average[[i-minK+1]] <- result.minkowski.metrics.average[[i-minK+1]]$avg.silwidth
  avg.sil.widths.correlation.average[[i-minK+1]] <- result.correlation.metrics.average[[i-minK+1]]$avg.silwidth
  
  chs.euclidean.average[[i-minK+1]] <- result.euclidean.metrics.average[[i-minK+1]]$ch
  chs.manhattan.average[[i-minK+1]] <- result.manhattan.metrics.average[[i-minK+1]]$ch
  chs.minkowski.average[[i-minK+1]] <- result.minkowski.metrics.average[[i-minK+1]]$ch
  chs.correlation.average[[i-minK+1]] <- result.correlation.metrics.average[[i-minK+1]]$ch
  
  dunns.euclidean.average[[i-minK+1]] <- result.euclidean.metrics.average[[i-minK+1]]$dunn
  dunns.manhattan.average[[i-minK+1]] <- result.manhattan.metrics.average[[i-minK+1]]$dunn
  dunns.minkowski.average[[i-minK+1]] <- result.minkowski.metrics.average[[i-minK+1]]$dunn
  dunns.correlation.average[[i-minK+1]] <- result.correlation.metrics.average[[i-minK+1]]$dunn
  
  # complete linkage
  cluster.ss.euclidean.complete[[i-minK+1]] <- result.euclidean.metrics.complete[[i-minK+1]]$within.cluster.ss
  cluster.ss.manhattan.complete[[i-minK+1]] <- result.manhattan.metrics.complete[[i-minK+1]]$within.cluster.ss
  cluster.ss.minkowski.complete[[i-minK+1]] <- result.minkowski.metrics.complete[[i-minK+1]]$within.cluster.ss
  cluster.ss.correlation.complete[[i-minK+1]] <- result.correlation.metrics.complete[[i-minK+1]]$within.cluster.ss
  
  avg.sil.widths.euclidean.complete[[i-minK+1]] <- result.euclidean.metrics.complete[[i-minK+1]]$avg.silwidth
  avg.sil.widths.manhattan.complete[[i-minK+1]] <- result.manhattan.metrics.complete[[i-minK+1]]$avg.silwidth
  avg.sil.widths.minkowski.complete[[i-minK+1]] <- result.minkowski.metrics.complete[[i-minK+1]]$avg.silwidth
  avg.sil.widths.correlation.complete[[i-minK+1]] <- result.correlation.metrics.complete[[i-minK+1]]$avg.silwidth
  
  chs.euclidean.complete[[i-minK+1]] <- result.euclidean.metrics.complete[[i-minK+1]]$ch
  chs.manhattan.complete[[i-minK+1]] <- result.manhattan.metrics.complete[[i-minK+1]]$ch
  chs.minkowski.complete[[i-minK+1]] <- result.minkowski.metrics.complete[[i-minK+1]]$ch
  chs.correlation.complete[[i-minK+1]] <- result.correlation.metrics.complete[[i-minK+1]]$ch
  
  dunns.euclidean.complete[[i-minK+1]] <- result.euclidean.metrics.complete[[i-minK+1]]$dunn
  dunns.manhattan.complete[[i-minK+1]] <- result.manhattan.metrics.complete[[i-minK+1]]$dunn
  dunns.minkowski.complete[[i-minK+1]] <- result.minkowski.metrics.complete[[i-minK+1]]$dunn
  dunns.correlation.complete[[i-minK+1]] <- result.correlation.metrics.complete[[i-minK+1]]$dunn
  
  #### supervised
  # average linkage
  rand.general.average[[i-minK+1]] <- result.euclidean.compare.general.average[[i-minK+1]]$corrected.rand
  rand.outfield.average[[i-minK+1]] <- result.euclidean.compare.outfield.average[[i-minK+1]]$corrected.rand
  rand.specific.average[[i-minK+1]] <- result.euclidean.compare.specific.average[[i-minK+1]]$corrected.rand
  
  vi.general.average[[i-minK+1]] <- result.euclidean.compare.general.average[[i-minK+1]]$vi
  vi.outfield.average[[i-minK+1]] <- result.euclidean.compare.outfield.average[[i-minK+1]]$vi
  vi.specific.average[[i-minK+1]] <- result.euclidean.compare.specific.average[[i-minK+1]]$vi
  
  # complete linkage
  rand.general.complete[[i-minK+1]] <- result.euclidean.compare.general.complete[[i-minK+1]]$corrected.rand
  rand.outfield.complete[[i-minK+1]] <- result.euclidean.compare.outfield.complete[[i-minK+1]]$corrected.rand
  rand.specific.complete[[i-minK+1]] <- result.euclidean.compare.specific.complete[[i-minK+1]]$corrected.rand
  
  vi.general.complete[[i-minK+1]] <- result.euclidean.compare.general.complete[[i-minK+1]]$vi
  vi.outfield.complete[[i-minK+1]] <- result.euclidean.compare.outfield.complete[[i-minK+1]]$vi
  vi.specific.complete[[i-minK+1]] <- result.euclidean.compare.specific.complete[[i-minK+1]]$vi
}

# average linkage
plot(clustersK, cluster.ss.euclidean.average, col="red", type="l")
plot(clustersK, cluster.ss.manhattan.average, col="blue", type="l")
plot(clustersK, cluster.ss.minkowski.average, col="green", type="l")
plot(clustersK, cluster.ss.correlation.average, col="black", type="l")
# complete linkage
plot(clustersK, cluster.ss.euclidean.complete, col="red", type="l")
plot(clustersK, cluster.ss.manhattan.complete, col="blue", type="l")
plot(clustersK, cluster.ss.minkowski.complete, col="green", type="l")
plot(clustersK, cluster.ss.correlation.complete, col="black", type="l")

# average linkage
plot(clustersK, avg.sil.widths.euclidean.average, col="red", ylim=c(0.0, 0.5), type="l")
points(clustersK, avg.sil.widths.manhattan.average, col="blue", type="l")
points(clustersK, avg.sil.widths.minkowski.average, col="green", type="l")
points(clustersK, avg.sil.widths.correlation.average, col="black", type="l")
# complete linkage
plot(clustersK, avg.sil.widths.euclidean.complete, col="red", ylim=c(0.0, 0.5), type="l")
points(clustersK, avg.sil.widths.manhattan.complete, col="blue", type="l")
points(clustersK, avg.sil.widths.minkowski.complete, col="green", type="l")
points(clustersK, avg.sil.widths.correlation.complete, col="black", type="l")

# average linkage
plot(clustersK, chs.euclidean.average, col="red", ylim=c(0.0, 8000), type="l")
points(clustersK, chs.manhattan.average, col="blue", type="l")
points(clustersK, chs.minkowski.average, col="green", type="l")
points(clustersK, chs.correlation.average, col="black", type="l")
# complete linkage
plot(clustersK, chs.euclidean.complete, col="red", ylim=c(0.0, 8000), type="l")
points(clustersK, chs.manhattan.complete, col="blue", type="l")
points(clustersK, chs.minkowski.complete, col="green", type="l")
points(clustersK, chs.correlation.complete, col="black", type="l")

# average linkage
plot(clustersK, dunns.euclidean.average, col="red", ylim=c(0.0, 0.08), type="l")
points(clustersK, dunns.manhattan.average, col="blue", type="l")
points(clustersK, dunns.minkowski.average, col="green", type="l")
points(clustersK, dunns.correlation.average, col="black", type="l")
# complete linkage
plot(clustersK, dunns.euclidean.complete, col="red", ylim=c(0.0, 0.08), type="l")
points(clustersK, dunns.manhattan.complete, col="blue", type="l")
points(clustersK, dunns.minkowski.complete, col="green", type="l")
points(clustersK, dunns.correlation.complete, col="black", type="l")

# average linkage
plot(clustersK, rand.general.average, ylim=c(0.0, 1.0), type="l", col="red")
lines(clustersK, rand.outfield.average, col="blue")
lines(clustersK, rand.specific.average, col="green")
# complete linkage
plot(clustersK, rand.general.complete, ylim=c(0.0, 1.0), type="l", col="red")
lines(clustersK, rand.outfield.complete, col="blue")
lines(clustersK, rand.specific.complete, col="green")

# average linkage
plot(clustersK, vi.general.average, ylim=c(0.0, 5.0), type="l", col="red")
lines(clustersK, vi.outfield.average, col="blue")
lines(clustersK, vi.specific.average, col="green")
# complete linkage
plot(clustersK, vi.general.complete, ylim=c(0.0, 5.0), type="l", col="red")
lines(clustersK, vi.outfield.complete, col="blue")
lines(clustersK, vi.specific.complete, col="green")
