library(mltools)
library(cluster)
library(dplyr)
library(proxy)
library(clv)
library(clusterCrit)
library(fpc)

load("../results/kmedoids-results-full.RData")
rm(result.correlation.centroids)
rm(result.correlation.clustering)
rm(result.correlation.pams)
rm(result.correlation.metrics)
rm(result.correlation.compare.general)
rm(result.correlation.compare.outfield)
rm(result.correlation.compare.specific)
load("../results/kmedoids-results-correlation-full.RData")

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


rand.general.euclidean <- list()
rand.outfield.euclidean <- list()
rand.specific.euclidean <- list()

rand.general.manhattan <- list()
rand.outfield.manhattan <- list()
rand.specific.manhattan <- list()

rand.general.minkowski <- list()
rand.outfield.minkowski <- list()
rand.specific.minkowski <- list()

rand.general.correlation <- list()
rand.outfield.correlation <- list()
rand.specific.correlation <- list()


vi.general.euclidean <- list()
vi.outfield.euclidean <- list()
vi.specific.euclidean <- list()

vi.general.manhattan <- list()
vi.outfield.manhattan <- list()
vi.specific.manhattan <- list()

vi.general.minkowski <- list()
vi.outfield.minkowski <- list()
vi.specific.minkowski <- list()

vi.general.correlation <- list()
vi.outfield.correlation <- list()
vi.specific.correlation <- list()


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
    rand.general.euclidean[[i-minK+1]] <- result.euclidean.compare.general[[i-minK+1]]$corrected.rand
    rand.outfield.euclidean[[i-minK+1]] <- result.euclidean.compare.outfield[[i-minK+1]]$corrected.rand
    rand.specific.euclidean[[i-minK+1]] <- result.euclidean.compare.specific[[i-minK+1]]$corrected.rand
    
    rand.general.manhattan[[i-minK+1]] <- result.manhattan.compare.general[[i-minK+1]]$corrected.rand
    rand.outfield.manhattan[[i-minK+1]] <- result.manhattan.compare.outfield[[i-minK+1]]$corrected.rand
    rand.specific.manhattan[[i-minK+1]] <- result.manhattan.compare.specific[[i-minK+1]]$corrected.rand
    
    rand.general.minkowski[[i-minK+1]] <- result.minkowski.compare.general[[i-minK+1]]$corrected.rand
    rand.outfield.minkowski[[i-minK+1]] <- result.minkowski.compare.outfield[[i-minK+1]]$corrected.rand
    rand.specific.minkowski[[i-minK+1]] <- result.minkowski.compare.specific[[i-minK+1]]$corrected.rand
    
    rand.general.correlation[[i-minK+1]] <- result.correlation.compare.general[[i-minK+1]]$corrected.rand
    rand.outfield.correlation[[i-minK+1]] <- result.correlation.compare.outfield[[i-minK+1]]$corrected.rand
    rand.specific.correlation[[i-minK+1]] <- result.correlation.compare.specific[[i-minK+1]]$corrected.rand
    
    vi.general.euclidean[[i-minK+1]] <- result.euclidean.compare.general[[i-minK+1]]$vi
    vi.outfield.euclidean[[i-minK+1]] <- result.euclidean.compare.outfield[[i-minK+1]]$vi
    vi.specific.euclidean[[i-minK+1]] <- result.euclidean.compare.specific[[i-minK+1]]$vi
    
    vi.general.manhattan[[i-minK+1]] <- result.manhattan.compare.general[[i-minK+1]]$vi
    vi.outfield.manhattan[[i-minK+1]] <- result.manhattan.compare.outfield[[i-minK+1]]$vi
    vi.specific.manhattan[[i-minK+1]] <- result.manhattan.compare.specific[[i-minK+1]]$vi
    
    vi.general.minkowski[[i-minK+1]] <- result.minkowski.compare.general[[i-minK+1]]$vi
    vi.outfield.minkowski[[i-minK+1]] <- result.minkowski.compare.outfield[[i-minK+1]]$vi
    vi.specific.minkowski[[i-minK+1]] <- result.minkowski.compare.specific[[i-minK+1]]$vi
    
    vi.general.correlation[[i-minK+1]] <- result.correlation.compare.general[[i-minK+1]]$vi
    vi.outfield.correlation[[i-minK+1]] <- result.correlation.compare.outfield[[i-minK+1]]$vi
    vi.specific.correlation[[i-minK+1]] <- result.correlation.compare.specific[[i-minK+1]]$vi
}

df <- data.frame(k=unlist(clustersK), 
                 ss.euclidean=unlist(cluster.ss.euclidean), 
                 ss.manhattan=unlist(cluster.ss.manhattan),
                 ss.minkowski=unlist(cluster.ss.minkowski),
                 ss.correlation=unlist(cluster.ss.correlation),
                 sil.euclidean=unlist(avg.sil.widths.euclidean),
                 sil.manhattan=unlist(avg.sil.widths.manhattan),
                 sil.minkowski=unlist(avg.sil.widths.minkowski),
                 sil.correlation=unlist(avg.sil.widths.correlation),
                 ch.euclidean=unlist(chs.euclidean),
                 ch.manhattan=unlist(chs.manhattan),
                 ch.minkowski=unlist(chs.minkowski),
                 ch.correlation=unlist(chs.correlation),
                 dunn.euclidean=unlist(dunns.euclidean),
                 dunn.manhattan=unlist(dunns.manhattan),
                 dunn.minkowski=unlist(dunns.minkowski),
                 dunn.correlation=unlist(dunns.correlation),
                 rand.general.euclidean=unlist(rand.general.euclidean),
                 rand.general.manhattan=unlist(rand.general.manhattan),
                 rand.general.minkowski=unlist(rand.general.minkowski),
                 rand.general.correlation=unlist(rand.general.correlation),
                 rand.outfield.euclidean=unlist(rand.outfield.euclidean),
                 rand.outfield.manhattan=unlist(rand.outfield.manhattan),
                 rand.outfield.minkowski=unlist(rand.outfield.minkowski),
                 rand.outfield.correlation=unlist(rand.outfield.correlation),
                 rand.specific.euclidean=unlist(rand.specific.euclidean),
                 rand.specific.manhattan=unlist(rand.specific.manhattan),
                 rand.specific.minkowski=unlist(rand.specific.minkowski),
                 rand.specific.correlation=unlist(rand.specific.correlation),
                 vi.general.euclidean=unlist(vi.general.euclidean),
                 vi.general.manhattan=unlist(vi.general.manhattan),
                 vi.general.minkowski=unlist(vi.general.minkowski),
                 vi.general.correlation=unlist(vi.general.correlation),
                 vi.outfield.euclidean=unlist(vi.outfield.euclidean),
                 vi.outfield.manhattan=unlist(vi.outfield.manhattan),
                 vi.outfield.minkowski=unlist(vi.outfield.minkowski),
                 vi.outfield.correlation=unlist(vi.outfield.correlation),
                 vi.specific.euclidean=unlist(vi.specific.euclidean),
                 vi.specific.manhattan=unlist(vi.specific.manhattan),
                 vi.specific.minkowski=unlist(vi.specific.minkowski),
                 vi.specific.correlation=unlist(vi.specific.correlation)
)

write.csv(df, "./kmedoid-full.csv")

load("../results/fanny-results-full.RData")
minK <- 2
maxK <- 40

clustersK<- list()
fsil.widths.euclidean <- list()
fsil.widths.manhattan <- list()
fsil.widths.minkowski <- list()
fsil.widths.correlation <- list()

for(i in minK:maxK){
    clustersK[[i-minK+1]] <- i
    # unsupervised
    fsil.widths.euclidean[[i-minK+1]] <- result.euclidean.metrics[[i-minK+1]]
    fsil.widths.manhattan[[i-minK+1]] <- result.manhattan.metrics[[i-minK+1]]
    fsil.widths.minkowski[[i-minK+1]] <- result.minkowski.metrics[[i-minK+1]]
    fsil.widths.correlation[[i-minK+1]] <- result.correlation.metrics[[i-minK+1]]
}

df <- data.frame(k=unlist(clustersK), 
                 fsil.euclidean=unlist(fsil.widths.euclidean), 
                 fsil.manhattan=unlist(fsil.widths.manhattan),
                 fsil.minkowski=unlist(fsil.widths.minkowski),
                 fsil.correlation=unlist(fsil.widths.correlation)
)

write.csv(df, "./cmeans-full.csv")

load("../results/cmeans-results-full.RData")


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


rand.general.euclidean <- list()
rand.outfield.euclidean <- list()
rand.specific.euclidean <- list()

rand.general.manhattan <- list()
rand.outfield.manhattan <- list()
rand.specific.manhattan <- list()

rand.general.minkowski <- list()
rand.outfield.minkowski <- list()
rand.specific.minkowski <- list()

rand.general.correlation <- list()
rand.outfield.correlation <- list()
rand.specific.correlation <- list()


vi.general.euclidean <- list()
vi.outfield.euclidean <- list()
vi.specific.euclidean <- list()

vi.general.manhattan <- list()
vi.outfield.manhattan <- list()
vi.specific.manhattan <- list()

vi.general.minkowski <- list()
vi.outfield.minkowski <- list()
vi.specific.minkowski <- list()

vi.general.correlation <- list()
vi.outfield.correlation <- list()
vi.specific.correlation <- list()


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
    rand.general.euclidean[[i-minK+1]] <- result.euclidean.compare.general[[i-minK+1]]$corrected.rand
    rand.outfield.euclidean[[i-minK+1]] <- result.euclidean.compare.outfield[[i-minK+1]]$corrected.rand
    rand.specific.euclidean[[i-minK+1]] <- result.euclidean.compare.specific[[i-minK+1]]$corrected.rand
    
    rand.general.manhattan[[i-minK+1]] <- result.manhattan.compare.general[[i-minK+1]]$corrected.rand
    rand.outfield.manhattan[[i-minK+1]] <- result.manhattan.compare.outfield[[i-minK+1]]$corrected.rand
    rand.specific.manhattan[[i-minK+1]] <- result.manhattan.compare.specific[[i-minK+1]]$corrected.rand
    
    rand.general.minkowski[[i-minK+1]] <- result.minkowski.compare.general[[i-minK+1]]$corrected.rand
    rand.outfield.minkowski[[i-minK+1]] <- result.minkowski.compare.outfield[[i-minK+1]]$corrected.rand
    rand.specific.minkowski[[i-minK+1]] <- result.minkowski.compare.specific[[i-minK+1]]$corrected.rand
    
    rand.general.correlation[[i-minK+1]] <- result.correlation.compare.general[[i-minK+1]]$corrected.rand
    rand.outfield.correlation[[i-minK+1]] <- result.correlation.compare.outfield[[i-minK+1]]$corrected.rand
    rand.specific.correlation[[i-minK+1]] <- result.correlation.compare.specific[[i-minK+1]]$corrected.rand
    
    vi.general.euclidean[[i-minK+1]] <- result.euclidean.compare.general[[i-minK+1]]$vi
    vi.outfield.euclidean[[i-minK+1]] <- result.euclidean.compare.outfield[[i-minK+1]]$vi
    vi.specific.euclidean[[i-minK+1]] <- result.euclidean.compare.specific[[i-minK+1]]$vi
    
    vi.general.manhattan[[i-minK+1]] <- result.manhattan.compare.general[[i-minK+1]]$vi
    vi.outfield.manhattan[[i-minK+1]] <- result.manhattan.compare.outfield[[i-minK+1]]$vi
    vi.specific.manhattan[[i-minK+1]] <- result.manhattan.compare.specific[[i-minK+1]]$vi
    
    vi.general.minkowski[[i-minK+1]] <- result.minkowski.compare.general[[i-minK+1]]$vi
    vi.outfield.minkowski[[i-minK+1]] <- result.minkowski.compare.outfield[[i-minK+1]]$vi
    vi.specific.minkowski[[i-minK+1]] <- result.minkowski.compare.specific[[i-minK+1]]$vi
    
    vi.general.correlation[[i-minK+1]] <- result.correlation.compare.general[[i-minK+1]]$vi
    vi.outfield.correlation[[i-minK+1]] <- result.correlation.compare.outfield[[i-minK+1]]$vi
    vi.specific.correlation[[i-minK+1]] <- result.correlation.compare.specific[[i-minK+1]]$vi
}

df <- data.frame(k=unlist(clustersK), 
                 ss.euclidean=unlist(cluster.ss.euclidean), 
                 ss.manhattan=unlist(cluster.ss.manhattan),
                 ss.minkowski=unlist(cluster.ss.minkowski),
                 ss.correlation=unlist(cluster.ss.correlation),
                 sil.euclidean=unlist(avg.sil.widths.euclidean),
                 sil.manhattan=unlist(avg.sil.widths.manhattan),
                 sil.minkowski=unlist(avg.sil.widths.minkowski),
                 sil.correlation=unlist(avg.sil.widths.correlation),
                 ch.euclidean=unlist(chs.euclidean),
                 ch.manhattan=unlist(chs.manhattan),
                 ch.minkowski=unlist(chs.minkowski),
                 ch.correlation=unlist(chs.correlation),
                 dunn.euclidean=unlist(dunns.euclidean),
                 dunn.manhattan=unlist(dunns.manhattan),
                 dunn.minkowski=unlist(dunns.minkowski),
                 dunn.correlation=unlist(dunns.correlation),
                 rand.general.euclidean=unlist(rand.general.euclidean),
                 rand.general.manhattan=unlist(rand.general.manhattan),
                 rand.general.minkowski=unlist(rand.general.minkowski),
                 rand.general.correlation=unlist(rand.general.correlation),
                 rand.outfield.euclidean=unlist(rand.outfield.euclidean),
                 rand.outfield.manhattan=unlist(rand.outfield.manhattan),
                 rand.outfield.minkowski=unlist(rand.outfield.minkowski),
                 rand.outfield.correlation=unlist(rand.outfield.correlation),
                 rand.specific.euclidean=unlist(rand.specific.euclidean),
                 rand.specific.manhattan=unlist(rand.specific.manhattan),
                 rand.specific.minkowski=unlist(rand.specific.minkowski),
                 rand.specific.correlation=unlist(rand.specific.correlation),
                 vi.general.euclidean=unlist(vi.general.euclidean),
                 vi.general.manhattan=unlist(vi.general.manhattan),
                 vi.general.minkowski=unlist(vi.general.minkowski),
                 vi.general.correlation=unlist(vi.general.correlation),
                 vi.outfield.euclidean=unlist(vi.outfield.euclidean),
                 vi.outfield.manhattan=unlist(vi.outfield.manhattan),
                 vi.outfield.minkowski=unlist(vi.outfield.minkowski),
                 vi.outfield.correlation=unlist(vi.outfield.correlation),
                 vi.specific.euclidean=unlist(vi.specific.euclidean),
                 vi.specific.manhattan=unlist(vi.specific.manhattan),
                 vi.specific.minkowski=unlist(vi.specific.minkowski),
                 vi.specific.correlation=unlist(vi.specific.correlation)
)

write.csv(df, "./diana-full.csv")
