library(mltools)
library(cluster)
library(dplyr)
library(proxy)
library(clv)
library(clusterCrit)
library(fpc)

# load original data from file
playersFull <- read.csv("./data/data.csv")
summary(playersFull)

#### clear the data from unnecessary columns and rows

# drop unnecessary columns
columnsToBeDropped = c("X", "ID", "Name", "Photo", "Nationality", "Flag", "Overall", 
                       "Potential", "Club", "Club.Logo", "Value", "Wage", 
                       "Special", "Real.Face", "Jersey.Number", "Joined",
                       "Loaned.From", "Contract.Valid.Until", "LS", "ST", "RS",
                       "LW", "LF", "CF", "RF", "RW", "LAM", "CAM", "RAM", "LM",
                       "LCM", "CM", "RCM", "RM", "LWB", "LDM", "CDM", "RDM", 
                       "RWB", "LB", "LCB", "CB", "RCB", "RB", "Release.Clause",
                       "International.Reputation")
playersCropped <- playersFull[!(names(playersFull) %in% columnsToBeDropped)]

# drop rows with missing values
playersCropped <- playersCropped[complete.cases(playersCropped),]

#### encode categorical variables and convert data
playersEncoded <- playersCropped

# binary encoding
playersEncoded$Preferred.Foot <- ifelse(playersEncoded$Preferred.Foot == "Left", 1 ,0) 

# split one attribute into two
playersEncoded[c("Work.Rate.Offensive", "Work.Rate.Defensive")] <- data.frame(do.call("rbind", strsplit(as.character(playersEncoded$Work.Rate), "/ ", fixed=TRUE)))
playersEncoded <- playersEncoded[(names(playersEncoded) != "Work.Rate")]

# apply ordinal encoding
workRateLevels <- c("Low", "Medium", "High")
playersEncoded$Work.Rate.Offensive <- as.numeric(factor(playersEncoded$Work.Rate.Offensive, levels=workRateLevels, ordered=TRUE))
playersEncoded$Work.Rate.Defensive <- as.numeric(factor(playersEncoded$Work.Rate.Defensive, levels=workRateLevels, ordered=TRUE))

# remove additional values and apply one-hot encoding
playersEncoded$Body.Type[playersEncoded$Body.Type == "Messi"] <- "Normal"
playersEncoded$Body.Type[playersEncoded$Body.Type == "C. Ronaldo"] <- "Stocky"
playersEncoded$Body.Type[playersEncoded$Body.Type == "Neymar"] <- "Lean"
playersEncoded$Body.Type[playersEncoded$Body.Type == "Courtois"] <- "Lean"
playersEncoded$Body.Type[playersEncoded$Body.Type == "PLAYER_BODY_TYPE_25"] <- "Normal"
playersEncoded$Body.Type[playersEncoded$Body.Type == "Shaqiri"] <- "Stocky"
playersEncoded$Body.Type[playersEncoded$Body.Type == "Akinfenwa"] <- "Stocky"
playersEncoded$Body.Type <- as.numeric(factor(playersEncoded$Body.Type, levels=c("Lean", "Normal", "Stocky")))

# convert height in ft to cm
z <- data.frame(do.call("rbind", strsplit(as.character(playersEncoded$Height), "'", fixed=TRUE)))
z <- modifyList(z, lapply(z[, sapply(z, is.factor)], function(x) as.numeric(as.character(x))))
playersEncoded$Height <- 0.3048 * z$X1 + 0.0254 * z$X2

# convert weight in lbs to kg
playersEncoded$Weight <- 0.45359237 * as.numeric(strsplit(as.character(playersEncoded$Weight), "lbs"))

# add BMI column
playersEncoded$BMI <- playersEncoded$Weight / (playersEncoded$Height * playersEncoded$Height)

#### prepare and scale attributes
playersNumeric <- playersEncoded
playersNumeric$Position <- as.numeric(factor(playersNumeric$Position))
playersPositions <- subset(playersEncoded, select=c("Position"))
playersData <- subset(playersEncoded, select=-c(Position))
playersData <- scale(playersData)

positionsAsText <- data.frame(lapply(playersPositions, as.character), stringsAsFactors=FALSE)

mapByOutfieldPosition <- function(x) {
    if(x == "GK"){
        return(1) 
    } else {
        return(2)
    }
}

mapByGeneralPosition <- function(x) {
    if(x %in% c("GK")){
        return(1)
    } else if(x %in% c("LB", "LCB", "CB", "RCB", "RB", "LWB", "RWB")){
        return(2)
    } else if(x %in% c("LM","LCM", "LDM", "CDM", "RDM", "LCM", "CM", "RCM", "RM", "LAM", "CAM", "RAM")){
        return(3)
    } else {
        return(4)
    }
}

outfieldTrueClustering <- apply(positionsAsText, 1, mapByOutfieldPosition)
generalTrueClustering <- apply(positionsAsText, 1, mapByGeneralPosition)
specificTrueClustering <- as.numeric(factor(playersNumeric$Position))

#### select final set of attributes
playersAttributesFinal <-as.data.frame(subset(playersData, select=c("Age", "Preferred.Foot", "Weak.Foot", 
                                                      "Crossing", "LongPassing", "Reactions", 
                                                      "Balance", "Penalties", "Work.Rate.Offensive", 
                                                      "Work.Rate.Defensive", "BMI", "Body.Type")))

GK.Skills <- c(apply(X=subset(playersData, select=c("GKDiving", "GKHandling", "GKKicking", "GKPositioning", "GKReflexes")), 
                                                     MARGIN=1, 
                                                     FUN=mean))

Tackling <- c(apply(X=subset(playersData, select=c("Interceptions", "StandingTackle", "SlidingTackle", "Aggression", "Marking")), 
                    MARGIN=1, 
                    FUN=mean))

Swiftness <- c(apply(X=subset(playersData, select=c("SprintSpeed", "Acceleration")), 
                     MARGIN=1, 
                     FUN=mean))

Short.Ball.Skills <- c(apply(X=subset(playersData, select=c("BallControl", "ShortPassing", "Dribbling", "Skill.Moves")), 
                             MARGIN=1, 
                             FUN=mean))

Intelligence <- c(apply(X=subset(playersData, select=c("Positioning", "Vision", "Composure")), 
                        MARGIN=1, 
                        FUN=mean))

Shooting <- c(apply(X=subset(playersData, select=c("Finishing", "Volleys", "LongShots")), 
                    MARGIN=1, 
                    FUN=mean))

Headers <- c(apply(X=subset(playersData, select=c("HeadingAccuracy", "Jumping")), 
                   MARGIN=1, 
                   FUN=mean))

Free.Kicks <- c(apply(X=subset(playersData, select=c("FKAccuracy", "Curve")), 
                      MARGIN=1, 
                      FUN=mean))

playersAttributesFinal <- cbind(playersAttributesFinal, 
                                GK.Skills, 
                                Tackling, 
                                Swiftness, 
                                Short.Ball.Skills, 
                                Intelligence,
                                Shooting, 
                                Headers, 
                                Free.Kicks)

### select random sample from the data
SAMPLE_RATIO <- 0.1
clusteringInput <- slice_sample(playersAttributesFinal, prop=SAMPLE_RATIO)
summary(clusteringInput)

### pam version -> k-medioids with euclidean distance
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

### 

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

####

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

save(result.correlation.centroids, 
     result.correlation.clustering,
     result.correlation.pams, 
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
     file="./kmedoids-results.RData"
)
