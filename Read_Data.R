baseballdata <- read.csv("2023_10_05_17_26_21_Rice_Pitching_Lab_44_Jackson_Mayo_Home.report (1).txt", sep="\t") # Default is sep=""
View(baseballdata)

n <- 0
MetricData <- data.frame(matrix(ncol = ncol(baseballdata), nrow = 5))
for (i in 1:ncol(baseballdata)) {
  if (baseballdata[2,i] == "METRIC") {
    n <- n+1
    MetricData[,n] <- baseballdata[1:5,i]
  }
}
MetricData <- Filter(function(x)!all(is.na(x)), MetricData) # Removes empty columns
View(MetricData)

n <- 0
EventData <- data.frame(matrix(ncol = ncol(baseballdata), nrow = 5))
for (i in 1:ncol(baseballdata)) {
  if (baseballdata[3,i] == "EVENT") {
    n <- n+1
    EventData[,n] <- baseballdata[1:5,i]
  }
}
EventData <- Filter(function(x)!all(is.na(x)), EventData) # Removes empty columns
View(EventData)

n <- 0
TSData <- data.frame(matrix(ncol = ncol(baseballdata), nrow = 5))
for (i in 1:ncol(baseballdata)) {
  if (baseballdata[3,i] == "TEMPORAL_SPATIAL") {
    n <- n+1
    TSData[,n] <- baseballdata[1:5,i]
  }
}
TSData <- Filter(function(x)!all(is.na(x)), TSData) # Removes empty columns
View(TSData)

n <- 1
DerivedData <- data.frame(matrix(ncol = ncol(baseballdata), nrow = nrow(baseballdata)))
for (i in 1:ncol(baseballdata)) {
  if (baseballdata[2,i] == "DERIVED") {
    n <- n+1
    DerivedData[,1] <- baseballdata[,1]
    DerivedData[,n] <- baseballdata[,i]
  }
}
DerivedData <- Filter(function(x)!all(is.na(x)), DerivedData) # Removes empty columns
View(DerivedData)

KinematicData <- data.frame(matrix(ncol = ncol(DerivedData), nrow = nrow(DerivedData)))
index <- which(baseballdata=="02_START_DATA", arr.ind=TRUE)
StartFrame <- as.numeric(baseballdata[5,index[1,2]])
IndexStartFrame <- which(DerivedData[,1]==StartFrame, arr.ind=TRUE)
KinematicData <- DerivedData[IndexStartFrame:nrow(DerivedData),]
toprows <- DerivedData[1:4,]
KinematicData <- rbind(toprows,KinematicData)
View(KinematicData)
