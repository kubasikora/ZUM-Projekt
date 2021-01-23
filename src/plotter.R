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

load("../results/cmeans-results-short.RData")
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

write.csv(df, "./cmeans-short.csv")

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


###########################################################################################################


load("../results/diana-results.RData")
minK <- 2
maxK <- 40

clustersK<- list()

cluster.ss.euclidean.average <- list()
cluster.ss.manhattan.average <- list()
cluster.ss.minkowski.average <- list()
cluster.ss.correlation.average <- list()


avg.sil.widths.euclidean.average <- list()
avg.sil.widths.manhattan.average <- list()
avg.sil.widths.minkowski.average <- list()
avg.sil.widths.correlation.average <- list()


chs.euclidean.average <- list()
chs.manhattan.average <- list()
chs.minkowski.average <- list()
chs.correlation.average <- list()


dunns.euclidean.average <- list()
dunns.manhattan.average <- list()
dunns.minkowski.average <- list()
dunns.correlation.average <- list()


rand.general.euclidean.average <- list()
rand.outfield.euclidean.average <- list()
rand.specific.euclidean.average <- list()

rand.general.manhattan.average <- list()
rand.outfield.manhattan.average <- list()
rand.specific.manhattan.average <- list()

rand.general.minkowski.average <- list()
rand.outfield.minkowski.average <- list()
rand.specific.minkowski.average <- list()

rand.general.correlation.average <- list()
rand.outfield.correlation.average <- list()
rand.specific.correlation.average <- list()


vi.general.euclidean.average <- list()
vi.outfield.euclidean.average <- list()
vi.specific.euclidean.average <- list()

vi.general.manhattan.average <- list()
vi.outfield.manhattan.average <- list()
vi.specific.manhattan.average <- list()

vi.general.minkowski.average <- list()
vi.outfield.minkowski.average <- list()
vi.specific.minkowski.average <- list()

vi.general.correlation.average <- list()
vi.outfield.correlation.average <- list()
vi.specific.correlation.average <- list()


for(i in minK:maxK){
    clustersK[[i-minK+1]] <- i
    # unsupervised
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
    
    # supervised
    rand.general.euclidean.average[[i-minK+1]] <- result.euclidean.compare.general.average[[i-minK+1]]$corrected.rand
    rand.outfield.euclidean.average[[i-minK+1]] <- result.euclidean.compare.outfield.average[[i-minK+1]]$corrected.rand
    rand.specific.euclidean.average[[i-minK+1]] <- result.euclidean.compare.specific.average[[i-minK+1]]$corrected.rand
    
    rand.general.manhattan.average[[i-minK+1]] <- result.manhattan.compare.general.average[[i-minK+1]]$corrected.rand
    rand.outfield.manhattan.average[[i-minK+1]] <- result.manhattan.compare.outfield.average[[i-minK+1]]$corrected.rand
    rand.specific.manhattan.average[[i-minK+1]] <- result.manhattan.compare.specific.average[[i-minK+1]]$corrected.rand
    
    rand.general.minkowski.average[[i-minK+1]] <- result.minkowski.compare.general.average[[i-minK+1]]$corrected.rand
    rand.outfield.minkowski.average[[i-minK+1]] <- result.minkowski.compare.outfield.average[[i-minK+1]]$corrected.rand
    rand.specific.minkowski.average[[i-minK+1]] <- result.minkowski.compare.specific.average[[i-minK+1]]$corrected.rand
    
    rand.general.correlation.average[[i-minK+1]] <- result.correlation.compare.general.average[[i-minK+1]]$corrected.rand
    rand.outfield.correlation.average[[i-minK+1]] <- result.correlation.compare.outfield.average[[i-minK+1]]$corrected.rand
    rand.specific.correlation.average[[i-minK+1]] <- result.correlation.compare.specific.average[[i-minK+1]]$corrected.rand
    
    vi.general.euclidean.average[[i-minK+1]] <- result.euclidean.compare.general.average[[i-minK+1]]$vi
    vi.outfield.euclidean.average[[i-minK+1]] <- result.euclidean.compare.outfield.average[[i-minK+1]]$vi
    vi.specific.euclidean.average[[i-minK+1]] <- result.euclidean.compare.specific.average[[i-minK+1]]$vi
    
    vi.general.manhattan.average[[i-minK+1]] <- result.manhattan.compare.general.average[[i-minK+1]]$vi
    vi.outfield.manhattan.average[[i-minK+1]] <- result.manhattan.compare.outfield.average[[i-minK+1]]$vi
    vi.specific.manhattan.average[[i-minK+1]] <- result.manhattan.compare.specific.average[[i-minK+1]]$vi
    
    vi.general.minkowski.average[[i-minK+1]] <- result.minkowski.compare.general.average[[i-minK+1]]$vi
    vi.outfield.minkowski.average[[i-minK+1]] <- result.minkowski.compare.outfield.average[[i-minK+1]]$vi
    vi.specific.minkowski.average[[i-minK+1]] <- result.minkowski.compare.specific.average[[i-minK+1]]$vi
    
    vi.general.correlation.average[[i-minK+1]] <- result.correlation.compare.general.average[[i-minK+1]]$vi
    vi.outfield.correlation.average[[i-minK+1]] <- result.correlation.compare.outfield.average[[i-minK+1]]$vi
    vi.specific.correlation.average[[i-minK+1]] <- result.correlation.compare.specific.average[[i-minK+1]]$vi
}

df <- data.frame(k=unlist(clustersK), 
                 ss.euclidean=unlist(cluster.ss.euclidean.average), 
                 ss.manhattan=unlist(cluster.ss.manhattan.average),
                 ss.minkowski=unlist(cluster.ss.minkowski.average),
                 ss.correlation=unlist(cluster.ss.correlation.average),
                 sil.euclidean=unlist(avg.sil.widths.euclidean.average),
                 sil.manhattan=unlist(avg.sil.widths.manhattan.average),
                 sil.minkowski=unlist(avg.sil.widths.minkowski.average),
                 sil.correlation=unlist(avg.sil.widths.correlation.average),
                 ch.euclidean=unlist(chs.euclidean.average),
                 ch.manhattan=unlist(chs.manhattan.average),
                 ch.minkowski=unlist(chs.minkowski.average),
                 ch.correlation=unlist(chs.correlation.average),
                 dunn.euclidean=unlist(dunns.euclidean.average),
                 dunn.manhattan=unlist(dunns.manhattan.average),
                 dunn.minkowski=unlist(dunns.minkowski.average),
                 dunn.correlation=unlist(dunns.correlation.average),
                 rand.general.euclidean=unlist(rand.general.euclidean.average),
                 rand.general.manhattan=unlist(rand.general.manhattan.average),
                 rand.general.minkowski=unlist(rand.general.minkowski.average),
                 rand.general.correlation=unlist(rand.general.correlation.average),
                 rand.outfield.euclidean=unlist(rand.outfield.euclidean.average),
                 rand.outfield.manhattan=unlist(rand.outfield.manhattan.average),
                 rand.outfield.minkowski=unlist(rand.outfield.minkowski.average),
                 rand.outfield.correlation=unlist(rand.outfield.correlation.average),
                 rand.specific.euclidean=unlist(rand.specific.euclidean.average),
                 rand.specific.manhattan=unlist(rand.specific.manhattan.average),
                 rand.specific.minkowski=unlist(rand.specific.minkowski.average),
                 rand.specific.correlation=unlist(rand.specific.correlation.average),
                 vi.general.euclidean=unlist(vi.general.euclidean.average),
                 vi.general.manhattan=unlist(vi.general.manhattan.average),
                 vi.general.minkowski=unlist(vi.general.minkowski.average),
                 vi.general.correlation=unlist(vi.general.correlation.average),
                 vi.outfield.euclidean=unlist(vi.outfield.euclidean.average),
                 vi.outfield.manhattan=unlist(vi.outfield.manhattan.average),
                 vi.outfield.minkowski=unlist(vi.outfield.minkowski.average),
                 vi.outfield.correlation=unlist(vi.outfield.correlation.average),
                 vi.specific.euclidean=unlist(vi.specific.euclidean.average),
                 vi.specific.manhattan=unlist(vi.specific.manhattan.average),
                 vi.specific.minkowski=unlist(vi.specific.minkowski.average),
                 vi.specific.correlation=unlist(vi.specific.correlation.average)
)

write.csv(df, "./diana-average-full.csv")



minK <- 2
maxK <- 40

clustersK<- list()

cluster.ss.euclidean.complete <- list()
cluster.ss.manhattan.complete <- list()
cluster.ss.minkowski.complete <- list()
cluster.ss.correlation.complete <- list()


avg.sil.widths.euclidean.complete <- list()
avg.sil.widths.manhattan.complete <- list()
avg.sil.widths.minkowski.complete <- list()
avg.sil.widths.correlation.complete <- list()


chs.euclidean.complete <- list()
chs.manhattan.complete <- list()
chs.minkowski.complete <- list()
chs.correlation.complete <- list()


dunns.euclidean.complete <- list()
dunns.manhattan.complete <- list()
dunns.minkowski.complete <- list()
dunns.correlation.complete <- list()


rand.general.euclidean.complete <- list()
rand.outfield.euclidean.complete <- list()
rand.specific.euclidean.complete <- list()

rand.general.manhattan.complete <- list()
rand.outfield.manhattan.complete <- list()
rand.specific.manhattan.complete <- list()

rand.general.minkowski.complete <- list()
rand.outfield.minkowski.complete <- list()
rand.specific.minkowski.complete <- list()

rand.general.correlation.complete <- list()
rand.outfield.correlation.complete <- list()
rand.specific.correlation.complete <- list()


vi.general.euclidean.complete <- list()
vi.outfield.euclidean.complete <- list()
vi.specific.euclidean.complete <- list()

vi.general.manhattan.complete <- list()
vi.outfield.manhattan.complete <- list()
vi.specific.manhattan.complete <- list()

vi.general.minkowski.complete <- list()
vi.outfield.minkowski.complete <- list()
vi.specific.minkowski.complete <- list()

vi.general.correlation.complete <- list()
vi.outfield.correlation.complete <- list()
vi.specific.correlation.complete <- list()


for(i in minK:maxK){
    clustersK[[i-minK+1]] <- i
    # unsupervised
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
    
    # supervised
    rand.general.euclidean.complete[[i-minK+1]] <- result.euclidean.compare.general.complete[[i-minK+1]]$corrected.rand
    rand.outfield.euclidean.complete[[i-minK+1]] <- result.euclidean.compare.outfield.complete[[i-minK+1]]$corrected.rand
    rand.specific.euclidean.complete[[i-minK+1]] <- result.euclidean.compare.specific.complete[[i-minK+1]]$corrected.rand
    
    rand.general.manhattan.complete[[i-minK+1]] <- result.manhattan.compare.general.complete[[i-minK+1]]$corrected.rand
    rand.outfield.manhattan.complete[[i-minK+1]] <- result.manhattan.compare.outfield.complete[[i-minK+1]]$corrected.rand
    rand.specific.manhattan.complete[[i-minK+1]] <- result.manhattan.compare.specific.complete[[i-minK+1]]$corrected.rand
    
    rand.general.minkowski.complete[[i-minK+1]] <- result.minkowski.compare.general.complete[[i-minK+1]]$corrected.rand
    rand.outfield.minkowski.complete[[i-minK+1]] <- result.minkowski.compare.outfield.complete[[i-minK+1]]$corrected.rand
    rand.specific.minkowski.complete[[i-minK+1]] <- result.minkowski.compare.specific.complete[[i-minK+1]]$corrected.rand
    
    rand.general.correlation.complete[[i-minK+1]] <- result.correlation.compare.general.complete[[i-minK+1]]$corrected.rand
    rand.outfield.correlation.complete[[i-minK+1]] <- result.correlation.compare.outfield.complete[[i-minK+1]]$corrected.rand
    rand.specific.correlation.complete[[i-minK+1]] <- result.correlation.compare.specific.complete[[i-minK+1]]$corrected.rand
    
    vi.general.euclidean.complete[[i-minK+1]] <- result.euclidean.compare.general.complete[[i-minK+1]]$vi
    vi.outfield.euclidean.complete[[i-minK+1]] <- result.euclidean.compare.outfield.complete[[i-minK+1]]$vi
    vi.specific.euclidean.complete[[i-minK+1]] <- result.euclidean.compare.specific.complete[[i-minK+1]]$vi
    
    vi.general.manhattan.complete[[i-minK+1]] <- result.manhattan.compare.general.complete[[i-minK+1]]$vi
    vi.outfield.manhattan.complete[[i-minK+1]] <- result.manhattan.compare.outfield.complete[[i-minK+1]]$vi
    vi.specific.manhattan.complete[[i-minK+1]] <- result.manhattan.compare.specific.complete[[i-minK+1]]$vi
    
    vi.general.minkowski.complete[[i-minK+1]] <- result.minkowski.compare.general.complete[[i-minK+1]]$vi
    vi.outfield.minkowski.complete[[i-minK+1]] <- result.minkowski.compare.outfield.complete[[i-minK+1]]$vi
    vi.specific.minkowski.complete[[i-minK+1]] <- result.minkowski.compare.specific.complete[[i-minK+1]]$vi
    
    vi.general.correlation.complete[[i-minK+1]] <- result.correlation.compare.general.complete[[i-minK+1]]$vi
    vi.outfield.correlation.complete[[i-minK+1]] <- result.correlation.compare.outfield.complete[[i-minK+1]]$vi
    vi.specific.correlation.complete[[i-minK+1]] <- result.correlation.compare.specific.complete[[i-minK+1]]$vi
}

df <- data.frame(k=unlist(clustersK), 
                 ss.euclidean=unlist(cluster.ss.euclidean.complete), 
                 ss.manhattan=unlist(cluster.ss.manhattan.complete),
                 ss.minkowski=unlist(cluster.ss.minkowski.complete),
                 ss.correlation=unlist(cluster.ss.correlation.complete),
                 sil.euclidean=unlist(avg.sil.widths.euclidean.complete),
                 sil.manhattan=unlist(avg.sil.widths.manhattan.complete),
                 sil.minkowski=unlist(avg.sil.widths.minkowski.complete),
                 sil.correlation=unlist(avg.sil.widths.correlation.complete),
                 ch.euclidean=unlist(chs.euclidean.complete),
                 ch.manhattan=unlist(chs.manhattan.complete),
                 ch.minkowski=unlist(chs.minkowski.complete),
                 ch.correlation=unlist(chs.correlation.complete),
                 dunn.euclidean=unlist(dunns.euclidean.complete),
                 dunn.manhattan=unlist(dunns.manhattan.complete),
                 dunn.minkowski=unlist(dunns.minkowski.complete),
                 dunn.correlation=unlist(dunns.correlation.complete),
                 rand.general.euclidean=unlist(rand.general.euclidean.complete),
                 rand.general.manhattan=unlist(rand.general.manhattan.complete),
                 rand.general.minkowski=unlist(rand.general.minkowski.complete),
                 rand.general.correlation=unlist(rand.general.correlation.complete),
                 rand.outfield.euclidean=unlist(rand.outfield.euclidean.complete),
                 rand.outfield.manhattan=unlist(rand.outfield.manhattan.complete),
                 rand.outfield.minkowski=unlist(rand.outfield.minkowski.complete),
                 rand.outfield.correlation=unlist(rand.outfield.correlation.complete),
                 rand.specific.euclidean=unlist(rand.specific.euclidean.complete),
                 rand.specific.manhattan=unlist(rand.specific.manhattan.complete),
                 rand.specific.minkowski=unlist(rand.specific.minkowski.complete),
                 rand.specific.correlation=unlist(rand.specific.correlation.complete),
                 vi.general.euclidean=unlist(vi.general.euclidean.complete),
                 vi.general.manhattan=unlist(vi.general.manhattan.complete),
                 vi.general.minkowski=unlist(vi.general.minkowski.complete),
                 vi.general.correlation=unlist(vi.general.correlation.complete),
                 vi.outfield.euclidean=unlist(vi.outfield.euclidean.complete),
                 vi.outfield.manhattan=unlist(vi.outfield.manhattan.complete),
                 vi.outfield.minkowski=unlist(vi.outfield.minkowski.complete),
                 vi.outfield.correlation=unlist(vi.outfield.correlation.complete),
                 vi.specific.euclidean=unlist(vi.specific.euclidean.complete),
                 vi.specific.manhattan=unlist(vi.specific.manhattan.complete),
                 vi.specific.minkowski=unlist(vi.specific.minkowski.complete),
                 vi.specific.correlation=unlist(vi.specific.correlation.complete)
)

write.csv(df, "./diana-complete-full.csv")

#######################################

load("../results/shorts/fanny-short-results-full.RData")
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

write.csv(df, "./fanny-short.csv")



load("../results/shorts/kmed/kmedoids-short-results-euclidean.RData")
load("../results/shorts/kmed/kmedoids-short-results-manhattan.RData")
load("../results/shorts/kmed/kmedoids-short-results-minkowski.RData")
load("../results/shorts/kmed/kmedoids-short-results-correlation.RData")
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

write.csv(df, "./kmedoids-short.csv")

load("../results/dbscan-results.RData")
df <- data.frame(k=result.dbscan.metrics$cluster.number,
                 ss=result.dbscan.metrics$within.cluster.ss,
                 sil=result.dbscan.metrics$avg.silwidth,
                 ch=result.dbscan.metrics$ch,
                 dunn=result.dbscan.metrics$dunn,
                 eps=cl_dbscan$eps,
                 minPts=cl_dbscan$minPts
)

write.csv(df, "./dbscan.csv")




load("./results/agnes-short/diana-results-shortened.RData")
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

write.csv(df, "./diana-short.csv")

load("../results/dbscan-results.RData")

clusters <- list()
noise <- list()
qualified <- list()
avgsize <- list()

minpts <- list()
eps <- list()

ss <- list()
sil <- list()
ch <- list()
dunn <- list()

for(i in 1:35){
    clusters[[i]] <- length(unique(cl_dbscan[[i]]$cluster)) - 1
    noise[[i]] <- sum(cl_dbscan[[i]]$cluster == 10000)
    qualified[[i]] <- length(cl_dbscan[[i]]$cluster) - noise[[i]]
    avgsize[[i]] <- mean(result.dbscan.metrics[[i]]$cluster.size[1:(length(result.dbscan.metrics[[i]]$cluster.size)-1)])
    minpts[[i]] <- cl_dbscan[[i]]$MinPts
    eps[[i]] <- cl_dbscan[[i]]$eps
    
    ss[[i]] <- result.dbscan.metrics[[i]]$within.cluster.ss
    sil[[i]] <- result.dbscan.metrics[[i]]$avg.silwidth
    ch[[i]] <- result.dbscan.metrics[[i]]$ch
    dunn[[i]] <- result.dbscan.metrics[[i]]$dunn
}


df <- data.frame(
    cluster=as.numeric(as.character(clusters)),
    noise=as.numeric(as.character(noise)),
    qualified=as.numeric(as.character(qualified)),
    avgsize=as.numeric(as.character(avgsize)),
    minpts=as.numeric(as.character(minpts)), 
    eps=as.numeric(as.character(eps)),
    ss=as.numeric(as.character(ss)),
    sil=as.numeric(as.character(sil)),
    ch=as.numeric(as.character(ch)),
    dunn=as.numeric(as.character(dunn))
)

write.csv(df, "./dbscan.csv")
