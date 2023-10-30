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
