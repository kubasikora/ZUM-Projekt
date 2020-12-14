library(mltools)
library(cluster)
library(dplyr)

# load original data from file
playersFull <- read.csv("../data/data.csv")
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


#### create true labels
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
