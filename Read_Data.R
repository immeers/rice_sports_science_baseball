install.packages("dplyr")
install.packages("tidyr")
library("dplyr")
library("tidyr")
library(ggplot2)
library(magrittr)



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
#baseballdata = read_data("2023_10_05_17_26_21_Rice_Pitching_Lab_44_Jackson_Mayo_Home.report (1).txt")

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
    
  KinematicData <- data.frame(matrix(ncol = ncol(derived_df), nrow = nrow(derived_df)))
  index <- which(baseballdata=="02_START_DATA", arr.ind=TRUE)
  StartFrame <- as.numeric(baseballdata[5,index[1,2]])
  IndexStartFrame <- which(derived_df[,1]==StartFrame, arr.ind=TRUE)
  KinematicData <- derived_df[IndexStartFrame:nrow(derived_df),]
  toprows <- derived_df[1:4,]
  KinematicData <- rbind(toprows,KinematicData)
  View(KinematicData)
  return(KinematicData)
}
kinematic_df = get_kinematic_data(baseballdata)

get_hand_kinematic_sequence <- function(baseballdata){
  
  index <- which(kinematic_df=="Pitching_Hand_KinematicSequence", arr.ind=TRUE)
  item <- kinematic_df[,1]
  handcolumn <- kinematic_df[,index[1,2]]
  hand_kinematic_data <- data.frame(item, as.numeric(handcolumn))
  colnames(hand_kinematic_data) <- c("Frame Number", "Hand Angular Velocity")
  hand_kinematic_data <- tail(hand_kinematic_data, -4)
  View(hand_kinematic_data)
  return(hand_kinematic_data)
  
}
hand_kinematic_sequence_df = get_hand_kinematic_sequence(baseballdata)

get_all_kinematic_sequences <- function(baseballdata) {
  s = 0
  columnnames = data.frame(matrix(ncol = ncol(kinematic_df), nrow = nrow(kinematic_df)))
  kinematic_sequences_data = data.frame(matrix(ncol = ncol(kinematic_df), nrow = nrow(kinematic_df)))
  item <- kinematic_df[5:nrow(kinematic_df),1]
  for (p in 1:ncol(kinematic_df)) {
    if (sapply("Kinematic", grepl, kinematic_df[1,p]) == "TRUE"){
      s = s + 1
      columnnames[1,s] = kinematic_df[1,p]
      kinematic_sequences_data[1:(nrow(kinematic_df)-4),s] = as.numeric(kinematic_df[5:nrow(kinematic_df),p])
    }
    
  }
  kinematic_sequences_data <- Filter(function(x)!all(is.na(x)), kinematic_sequences_data)
  kinematic_sequences_data <- kinematic_sequences_data[1:(nrow(kinematic_sequences_data)-4),]
  kinematic_sequences_data_frame <- data.frame(item, kinematic_sequences_data)
  colnames(kinematic_sequences_data_frame) <- c("FrameNumber", columnnames[1,1], columnnames[1,2], columnnames[1,3], columnnames[1,4])
  View(kinematic_sequences_data_frame)
  return(kinematic_sequences_data_frame)
}
all_kinematic_sequences_df = get_all_kinematic_sequences(baseballdata)

get_max_hand_vel_frame <- function(baseballdata){
  max_hand_vel <- max(hand_kinematic_sequence_df$`Hand Angular Velocity`, na.rm = TRUE)
  i <- which(hand_kinematic_sequence_df==max_hand_vel, arr.ind=TRUE)
  max_hand_vel_frame <- hand_kinematic_sequence_df[i[1],1]
  return(max_hand_vel_frame)
}

max_hand_velocity_frame = get_max_hand_vel_frame(baseballdata)

get_keyframe_data <- function(filename){
  keyframedata <- read.csv(filename, sep=",") # Default is sep=""
  KeyFrameEvent <- c("Start", "MaximumLeadingLegLift", "HandsApart", "ArmsOut", "LeadingFootStrike", "MaximumExternalShoulderRotation", "Acceleration", "BallRelease", "MaximumInternalShoulderRotation", "Deceleration", "MaximumTrailingLegLift", "End")
  
  keyframedata$AreKeyFramesDetected <- gsub('[\\{\\}]', '', keyframedata$AreKeyFramesDetected)
  AreKeyFramesDetected <- c(strsplit(keyframedata$AreKeyFramesDetected, ";"))
  
  keyframedata$KeyFrameIndices <- gsub('[\\{\\}]', '', keyframedata$KeyFrameIndices)
  KeyFrameIndices <- c(strsplit(keyframedata$KeyFrameIndices, ";"))
  
  keyframedata$KeyFrameDetectionScores <- gsub('[\\{\\}]', '', keyframedata$KeyFrameDetectionScores)
  KeyFrameDetectionScores <- c(strsplit(keyframedata$KeyFrameDetectionScores, ";"))
  
  Tracked <- c(keyframedata$Tracked)
  
  KeyFrameDF <- data.frame(KeyFrameEvent, AreKeyFramesDetected, KeyFrameIndices, KeyFrameDetectionScores, Tracked)
  colnames(KeyFrameDF) = c("KeyFrameEvent", "AreKeyFramesDetected", "KeyFrameIndices", "KeyFrameDetectionScores", "Tracked")
  View(KeyFrameDF)
  return(KeyFrameDF)
}
keyframe_df = get_keyframe_data("motion_tracker_result_parameters.csv")

if (keyframe_df$KeyFrameIndices[8] != max_hand_velocity_frame){
  print("Ball Release Not Concurrent With Max Hand Angular Velocity")
}

ggplot(data = hand_kinematic_sequence_df, aes(x = `Frame Number`, y = `Hand Angular Velocity`)) + geom_point() + geom_vline(xintercept=keyframe_df$KeyFrameIndices, linetype="solid", colour = "red")

longkinematicdf = all_kinematic_sequences_df %>% pivot_longer(cols=c('Pelvis_KinematicSequence', 'Trunk_KinematicSequence', 'Pitching_UpperArm_KinematicSequence', 'Pitching_Hand_KinematicSequence'), names_to='KinematicSequence',values_to='AngularVelocity')
ggplot(data = longkinematicdf, aes(x = FrameNumber, y = AngularVelocity)) + geom_point(aes(color=KinematicSequence)) + geom_vline(xintercept=keyframe_df$KeyFrameIndices, linetype="solid", colour = "red")

#PitchInfo <- read.csv(("Mayo_Fastball_85.6_15.1_-9.3_TM_Metadata.txt"), sep="\t")
PitchInfo <- read.csv(("C:/Users/immim/OneDrive/Rice/Sports Science/pitching data/Mayo_Fastball_85.6_15.1_-9.3_TM_Metadata.txt"), sep="\t")

View(PitchInfo)
