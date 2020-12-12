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
maxK = 30

print("Euclidean")
euclideanDistanceMatrix <- dist(clusteringInput, method="Euclidean")
euclideanDistanceMatrixFull <- dist(playersAttributesFinal, method="Euclidean")
result.euclidean.pams <- list()
result.euclidean.centroids <- list()
result.euclidean.clustering <- list()
result.euclidean.metrics <- list()

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
    print(paste("Evaluated metrics for", i, "clusters"))
}

### pam version -> k-medioids with minkowski distance with p = 3

print("Minkowski")
minkowskiDistanceMatrix <- dist(clusteringInput, method="Minkowski", p=3)
minkowskiDistanceMatrixFull <- dist(playersAttributesFinal, method="Minkowski", p=3)
result.minkowski.pams <- list()
result.minkowski.centroids <- list()
result.minkowski.clustering <- list()
result.minkowski.metrics <- list()

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
    print(paste("Evaluated metrics for", i, "clusters"))
}

### 

print("Manhattan")
manhattanDistanceMatrix <- dist(clusteringInput, method="Manhattan")
manhattanDistanceMatrixFull <- dist(playersAttributesFinal, method="Manhattan")
result.manhattan.pams <- list()
result.manhattan.centroids <- list()
result.manhattan.clustering <- list()
result.manhattan.metrics <- list()

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
    print(paste("Evaluated metrics for", i, "clusters"))
}

####

print("Correlation")
correlationDistanceMatrix <- dist(clusteringInput, method="correlation")
correlationDistanceMatrixFull <- dist(playersAttributesFinal, method="correlation")
result.correlation.pams <- list()
result.correlation.centroids <- list()
result.correlation.clustering <- list()
result.correlation.metrics <- list()

for(i in minK:maxK){
    # find initial clusters for sampled data.frame
    result <- pam(euclideanDistanceMatrix, i, diss=TRUE, pamonce=5, keep.diss=TRUE)
    print(paste("Found ", i, "clusters"))
    
    result.correlation.pams[[i-minK+1]] <- result
    result.correlation.centroids[[i-minK+1]] <- result$medoids
    
    # find clusters for all examples
    medoids <- playersAttributesFinal[result$medoids,]
    distances <- dist(playersAttributesFinal, medoids, method="Euclidean")
    result.correlation.clustering[[i-minK+1]] <- apply(distances, 1, which.min)
    print(paste("Found clusters for all examples"))
    
    # find metric values 
    result.correlation.metrics[[i-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.correlation.clustering[[i-minK+1]])
    print(paste("Evaluated metrics for", i, "clusters"))
}

## print(result.euclidean.metrics[[1]]$dunn)