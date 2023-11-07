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
