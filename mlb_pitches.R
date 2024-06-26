install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
library("dplyr")
library("tidyr")
library(ggplot2)
library(magrittr)
library(stringr)

#' read_data
#'
#' Function that reads data into a data frame
#' @param filename path of file as string
#' @return returns df of data in file
#' @examples
read_data <- function(filename, date){
  baseballdata <- read.csv(filename, sep="\t") # Default is sep=""
  #View(baseballdata)
  return(baseballdata)
}

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
  #View(MetricData)
  return(MetricData)
}

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
  
  #View(transposed_df)
  
  return(transposed_df)
}

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
  #View(EventData)
  return(EventData)
}


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
  return(TSData)
}

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
  #View(DerivedData)
  return(DerivedData)
}


get_kinematic_data <-function(baseballdata){
  
  KinematicData <- data.frame(matrix(ncol = ncol(derived_df), nrow = nrow(derived_df)))
  index <- which(baseballdata=="02_START_DATA", arr.ind=TRUE)
  StartFrame <- as.numeric(baseballdata[5,index[1,2]])
  IndexStartFrame <- which(derived_df[,1]==StartFrame, arr.ind=TRUE)
  KinematicData <- derived_df[IndexStartFrame:nrow(derived_df),]
  toprows <- derived_df[1:4,]
  KinematicData <- rbind(toprows,KinematicData)
  #View(KinematicData)
  return(KinematicData)
}

get_hand_kinematic_sequence <- function(baseballdata, date_str){
  
  index <- which(kinematic_df=="Pitching_Hand_KinematicSequence", arr.ind=TRUE)
  item <- kinematic_df[,1]
  handcolumn <- kinematic_df[,index[1,2]]
  hand_kinematic_data <- data.frame(as.numeric(item), as.numeric(handcolumn))
  colnames(hand_kinematic_data) <- c("FrameNumber", paste("Hand Angular Velocity", date_str))
  hand_kinematic_data <- tail(hand_kinematic_data, -4)
  #View(hand_kinematic_data)
  return(hand_kinematic_data)
  
}

get_pelvis_kinematic_sequence <- function(baseballdata, date_str){
  
  index <- which(kinematic_df=="Pelvis_KinematicSequence", arr.ind=TRUE)
  item <- kinematic_df[,1]
  pelviscolumn <- kinematic_df[,index[1,2]]
  pelvis_kinematic_data <- data.frame(as.numeric(item), as.numeric(pelviscolumn))
  colnames(pelvis_kinematic_data) <- c("FrameNumber", paste("Pelvis Angular Velocity", date_str))
  pelvis_kinematic_data <- tail(pelvis_kinematic_data, -4)
  #View(pelvis_kinematic_data)
  return(pelvis_kinematic_data)
  
}

get_trunk_kinematic_sequence <- function(baseballdata, date_str){
  
  index <- which(kinematic_df=="Trunk_KinematicSequence", arr.ind=TRUE)
  item <- kinematic_df[,1]
  trunkcolumn <- kinematic_df[,index[1,2]]
  trunk_kinematic_data <- data.frame(as.numeric(item), as.numeric(trunkcolumn))
  colnames(trunk_kinematic_data) <- c("FrameNumber", paste("Trunk Angular Velocity", date_str))
  trunk_kinematic_data <- tail(trunk_kinematic_data, -4)
  #View(trunk_kinematic_data)
  return(trunk_kinematic_data)
  
}

get_upperarm_kinematic_sequence <- function(baseballdata, date_str){
  
  index <- which(kinematic_df=="Pitching_UpperArm_KinematicSequence", arr.ind=TRUE)
  item <- kinematic_df[,1]
  upperarmcolumn <- kinematic_df[,index[1,2]]
  upperarm_kinematic_data <- data.frame(as.numeric(item), as.numeric(upperarmcolumn))
  colnames(upperarm_kinematic_data) <- c("FrameNumber", paste("Upper Arm Angular Velocity", date_str))
  upperarm_kinematic_data <- tail(upperarm_kinematic_data, -4)
  #View(upperarm_kinematic_data)
  return(upperarm_kinematic_data)
  
}

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

get_max_hand_vel_frame <- function(baseballdata){
  max_hand_vel <- max(hand_kinematic_sequence_df$`Hand Angular Velocity`, na.rm = TRUE)
  i <- which(hand_kinematic_sequence_df==max_hand_vel, arr.ind=TRUE)
  max_hand_vel_frame <- hand_kinematic_sequence_df[i[1],1]
  return(max_hand_vel_frame)
}

get_keyframe_data <- function(filename){
  keyframedata <- read.csv(filename, sep=",") # Default is sep=""
  View(keyframedata)
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

# Specify the directory path
directory_path <- "C:/Users/immim/OneDrive/Rice/Sports Science/to be analyzed/to be analyzed/"
#directory_path <- "C:/Users/ahaly/OneDrive/Documents/rice_sports_science_baseball/"

# Get a list of file names in the directory
files <- list.files(path = directory_path)
#can just type files in console
View(files)

# Loop over each file
i = 0
date_df <- data.frame(stringsAsFactors = FALSE)
date_df[1, ] <- NA

for (file in files) {
  # Construct the full path to the file
  full_path <- file.path(directory_path, file)
  i = i + 1
  print(i)
  
  
  # Define the regex pattern
  pattern <- "(\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2})"
  
  # Use str_extract to extract the matching pattern
  date <- str_extract(file, pattern)
  
  # Print the result
  
  file_extension <- tools::file_ext(file)
  
  if (file_extension %in% c("txt", "TXT")) {
    # CSV file
    #read_data(file, date)
    print(date)
    print(file)  


    
    baseballdata = read_data(full_path, date)
    #baseballdata = read_data("2023_10_05_17_26_21_Rice_Pitching_Lab_44_Jackson_Mayo_Home.report (1).txt")
    
    metric_df = get_metric_data(baseballdata)
    
    metric_df = transform_metric_data(metric_df)
    

    event_df = get_event_data(baseballdata)
    

    ts_df = get_ts_data(baseballdata)
    

    derived_df = get_derived_data(baseballdata)
    

    kinematic_df = get_kinematic_data(baseballdata)
    

    hand_kinematic_sequence_df = get_hand_kinematic_sequence(baseballdata, date)
    

    pelvis_kinematic_sequence_df = get_pelvis_kinematic_sequence(baseballdata, date)
    

    trunk_kinematic_sequence_df = get_trunk_kinematic_sequence(baseballdata, date)
    

    upperarm_kinematic_sequence_df = get_upperarm_kinematic_sequence(baseballdata, date)
    

    all_kinematic_sequences_df = get_all_kinematic_sequences(baseballdata)
    

    
    max_hand_velocity_frame = get_max_hand_vel_frame(baseballdata)
    

    #keyframe_df = get_keyframe_data('C:/Users/immim/OneDrive/Rice/Sports Science/to be analyzed/to be analyzed/2023_12_22_09_48_29_Rice_Pitching_Lab_0_Matt_Canterino_motion_tracker_frame_result_parameters.csv')
    #if (keyframe_df$KeyFrameIndices[8] != max_hand_velocity_frame){
    #  print("Ball Release Not Concurrent With Max Hand Angular Velocity")
    #}
    #ggplot(data = hand_kinematic_sequence_df, aes(x = `Frame Number`, y = `Hand Angular Velocity`)) + geom_point() + geom_vline(xintercept=keyframe_df$KeyFrameIndices, linetype="solid", colour = "red")
    
    #pivots cols in all kinematic sequences
    longkinematicdf = all_kinematic_sequences_df %>% pivot_longer(cols=c('Pelvis_KinematicSequence', 'Trunk_KinematicSequence', 'Pitching_UpperArm_KinematicSequence', 'Pitching_Hand_KinematicSequence'), names_to='KinematicSequence',values_to='AngularVelocity')
    #View(longkinematicdf)
    #ggplot(data = longkinematicdf, aes(x = FrameNumber, y = AngularVelocity)) + geom_point(aes(color=KinematicSequence)) + geom_vline(xintercept=keyframe_df$KeyFrameIndices, linetype="solid", colour = "red")
    
    #PitchInfo <- read.csv(("Mayo_Fastball_85.6_15.1_-9.3_TM_Metadata.txt"), sep="\t")
    #PitchInfo <- read.csv(("C:/Users/immim/OneDrive/Rice/Sports Science/pitching data/Mayo_Fastball_85.6_15.1_-9.3_TM_Metadata.txt"), sep="\t")
    
    #View(PitchInfo)
    #View(all_kinematic_sequences_df)
    
    ##truncate na
    ##!(is.na(newdf[,2]))- get 
    ##na.omit(new_df)- tidyverse df with no nas
    
    ##reset frame numbers
    ##get rid of frame numbers
    
    ##normalise
    ##key event = ball release
    ##down sampling 1700 -> 1000 downsample() groupdata2, scales to smallest col size
    ##interpolation- spline: approx a curve, linear: join dot to dot
    ##before add to master, normalise (interpolate to 105 rows), create x = 0 to 105
    
    ##geom_vline (xintercept = ballrelease)
    ##df norm len = 80, pre release
    ##df norm len = 30, post release
    ##stack = 105
    ##Normalise pre and post ball release
    ##diff between curves- show how consistent, magnitude ^2 sqrt or 
    
    new_df <- data.frame()
    
    currentdate_df <- data.frame(date)
    date_df <- cbind(date_df, currentdate_df)
    print(i)
    if (i == 1){
      master_hand_df <- rbind(new_df, hand_kinematic_sequence_df)
      master_pelvis_df <- rbind(new_df, pelvis_kinematic_sequence_df)
      master_trunk_df <- rbind(new_df, trunk_kinematic_sequence_df)
      master_upperarm_df <- rbind(new_df, upperarm_kinematic_sequence_df)
      print("here")
    } else {
      master_hand_df <- merge(master_hand_df, hand_kinematic_sequence_df, by = "FrameNumber", all = TRUE)
      master_pelvis_df <- merge(master_pelvis_df, pelvis_kinematic_sequence_df, by = "FrameNumber", all = TRUE)
      master_trunk_df <- merge(master_trunk_df, trunk_kinematic_sequence_df, by = "FrameNumber", all = TRUE)
      master_upperarm_df <- merge(master_upperarm_df, upperarm_kinematic_sequence_df, by = "FrameNumber", all = TRUE)
    }
    

  
  }
  
}
View(master_hand_df)
#View(master_pelvis_df)
#View(master_trunk_df)
#View(master_upperarm_df)

# Pivots the data frames for each joint

longhanddf = master_hand_df %>% pivot_longer(cols=c(paste("Hand Angular Velocity", date_df[1,1]), paste("Hand Angular Velocity", date_df[1,2]), paste("Hand Angular Velocity", date_df[1,3]), paste("Hand Angular Velocity", date_df[1,4])), names_to='KinematicSequence',values_to='AngularVelocity')
longtrunkdf = master_trunk_df %>% pivot_longer(cols=c(paste("Trunk Angular Velocity", date_df[1,1]), paste("Trunk Angular Velocity", date_df[1,2]), paste("Trunk Angular Velocity", date_df[1,3]), paste("Trunk Angular Velocity", date_df[1,4])), names_to='KinematicSequence',values_to='AngularVelocity')
longpelvisdf = master_pelvis_df %>% pivot_longer(cols=c(paste("Pelvis Angular Velocity", date_df[1,1]), paste("Pelvis Angular Velocity", date_df[1,2]), paste("Pelvis Angular Velocity", date_df[1,3]), paste("Pelvis Angular Velocity", date_df[1,4])), names_to='KinematicSequence',values_to='AngularVelocity')
longupperarmdf = master_upperarm_df %>% pivot_longer(cols=c(paste("Upper Arm Angular Velocity", date_df[1,1]), paste("Upper Arm Angular Velocity", date_df[1,2]), paste("Upper Arm Angular Velocity", date_df[1,3]), paste("Upper Arm Angular Velocity", date_df[1,4])), names_to='KinematicSequence',values_to='AngularVelocity')
View(longhanddf)
# Plots the points
ggplot(data = longhanddf, aes(x = FrameNumber, y = AngularVelocity)) + geom_point(aes(color=KinematicSequence))
ggplot(data = longtrunkdf, aes(x = FrameNumber, y = AngularVelocity)) + geom_point(aes(color=KinematicSequence))
ggplot(data = longpelvisdf, aes(x = FrameNumber, y = AngularVelocity)) + geom_point(aes(color=KinematicSequence))
ggplot(data = longupperarmdf, aes(x = FrameNumber, y = AngularVelocity)) + geom_point(aes(color=KinematicSequence))

# Plots the curves
ggplot(data = longhanddf, aes(x = FrameNumber, y = AngularVelocity, color = KinematicSequence)) +
  geom_line() +
  labs(x = "Frame Number", y = "Angular Velocity", title = "Hand Angular Velocity Plot by Date")
ggplot(data = longtrunkdf, aes(x = FrameNumber, y = AngularVelocity, color = KinematicSequence)) +
  geom_line() +
  labs(x = "Frame Number", y = "Angular Velocity", title = "Trunk Angular Velocity Plot by Date")
ggplot(data = longpelvisdf, aes(x = FrameNumber, y = AngularVelocity, color = KinematicSequence)) +
  geom_line() +
  labs(x = "Frame Number", y = "Angular Velocity", title = "Pelvis Angular Velocity Plot by Date")
ggplot(data = longupperarmdf, aes(x = FrameNumber, y = AngularVelocity, color = KinematicSequence)) +
  geom_line() +
  labs(x = "Frame Number", y = "Angular Velocity", title = "Upper Arm Angular Velocity Plot by Date")

# Plots the smoothed curves
ggplot(data = longhanddf, aes(x = FrameNumber, y = AngularVelocity, color = KinematicSequence)) +
  geom_smooth() +
  labs(x = "Frame Number", y = "Angular Velocity", title = "Smoothed Hand Angular Velocity Plot by Date")

