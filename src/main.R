library(data.table)
library(measurements)
library(mltools)

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
playersEncoded[c("Body.Type.Lean", "Body.Type.Normal", "Body.Type.Stocky")] <- one_hot(as.data.table(factor(as.numeric(playersEncoded$Body.Type), labels=c("Lean", "Normal", "Stocky"))))
playersEncoded <- subset(playersEncoded, select=-c(Body.Type))

# convert height in ft to cm
z <- data.frame(do.call("rbind", strsplit(as.character(playersEncoded$Height), "'", fixed=TRUE)))
z <- modifyList(z, lapply(z[, sapply(z, is.factor)], function(x) as.numeric(as.character(x))))
playersEncoded$Height <- 0.3048 * z$X1 + 0.0254 * z$X2

# convert weight in lbs to kg
playersEncoded$Weight <- 0.45359237 * as.numeric(strsplit(as.character(playersEncoded$Weight), "lbs"))



#### prepare 
playersPositions <- subset(playersEncoded, select=c("Position"))
playersData <- subset(playersEncoded, select=-c(Position))

#### scale attributes
playersData <- scale(playersData)

km.result <- kmeans(playersData, 4, iter.max=100, nstart=25)

#If you want to add the point classifications to the original data, use this: 
dd <- cbind(playersPositions, cluster=km.result$cluster)
View(dd)


