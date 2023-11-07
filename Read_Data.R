install.packages("dplyr")
install.packages("tidyr")
library("dplyr")
library("tidyr")


#' read_data
#'
#' Function that reads data into a data frame
#' @param filename path of file as string
#' @return returns df of data in file
#' @examples
read_data <- function(filename){
  baseballdata <- read.csv(filename, sep="\t") # Default is sep=""
  View(baseballdata)
  return(baseballdata)
}
baseballdata = read_data('C:/Users/immim/OneDrive/Rice/Sports Science/pitching data/2023_10_05_17_26_21_Rice_Pitching_Lab_44_Jackson_Mayo_Home.report.txt')

#' get_metric_data
#'
#' Function that metric data from previously read data frame
#' @param data frame to extract from
#' @return returns df of metric data
#' @examples
get_metric_data <-function(baseballdata){
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
  return(MetricData)
}

#call above function
metric_df = get_metric_data(baseballdata)

#melted_df <- metric_df %>%
 # pivot_longer(everything(), names_to = "variable", values_to = "value")

#View(melted_df)


#' transform_metric_data
#'
#' Function that transforms wide table into deep tabe
#' @param data frame of metric data
#' @return returns transformed df
#' @examples
transform_metric_data <- function(metric_df){
  #transpose
  transposed_df <- t(metric_df)
  transposed_df <- as.data.frame(transposed_df)
  
  #rename cols
  colnames(transposed_df) = c("variable", "type", "subtype", "axis", "reading")
  
  #new col for UOM
  transposed_df$UOM <- NA
  
  View(transposed_df)
  
  return(transposed_df)
}

metric_df = transform_metric_data(metric_df)

get_event_data <-function(baseballdata){
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
  return(EventData)
}
event_df = get_event_data(baseballdata)

get_ts_data <-function(baseballdata){
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
  return(TSData)
}
ts_df = get_ts_data(baseballdata)

get_derived_data <-function(baseballdata){
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
  return(DerivedData)
}
derived_df = get_derived_data(baseballdata)

get_kinematic_data <-function(baseballdata){
    
  KinematicData <- data.frame(matrix(ncol = ncol(DerivedData), nrow = nrow(DerivedData)))
  index <- which(baseballdata=="02_START_DATA", arr.ind=TRUE)
  StartFrame <- as.numeric(baseballdata[5,index[1,2]])
  IndexStartFrame <- which(DerivedData[,1]==StartFrame, arr.ind=TRUE)
  KinematicData <- DerivedData[IndexStartFrame:nrow(DerivedData),]
  toprows <- DerivedData[1:4,]
  KinematicData <- rbind(toprows,KinematicData)
  View(KinematicData)
  return(KinematicData)
}
kinematic_df = get_kinematic_data(baseballdata)
