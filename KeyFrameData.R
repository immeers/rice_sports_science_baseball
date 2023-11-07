keyframedata <- read.csv("motion_tracker_result_parameters.csv", sep=",") # Default is sep=""
View(keyframedata)
KeyFrameEvent <- c("Start", "MaximumLeadingLegLift", "HandsApart", "ArmsOut", "LeadingFootStrike", "MaximumExternalShoulderRotation", "Acceleration", "BallRelease", "MaximumInternalShoulderRotation", "Deceleration", "MaximumTrailingLegLift", "End")

keyframedata$AreKeyFramesDetected <- gsub('\\{', '', keyframedata$AreKeyFramesDetected)
keyframedata$AreKeyFramesDetected <- gsub('\\}', '', keyframedata$AreKeyFramesDetected)
AreKeyFramesDetected <- c(strsplit(keyframedata$AreKeyFramesDetected, ";"))

keyframedata$KeyFrameIndices <- gsub('\\{', '', keyframedata$KeyFrameIndices)
keyframedata$KeyFrameIndices <- gsub('\\}', '', keyframedata$KeyFrameIndices)
KeyFrameIndices <- c(strsplit(keyframedata$KeyFrameIndices, ";"))

keyframedata$KeyFrameDetectionScores <- gsub('\\{', '', keyframedata$KeyFrameDetectionScores)
keyframedata$KeyFrameIndices <- gsub('\\}', '', keyframedata$KeyFrameDetectionScores)
KeyFrameDetectionScores <- c(strsplit(keyframedata$KeyFrameDetectionScores, ";"))

KeyFrameDF <- data.frame(KeyFrameEvent, AreKeyFramesDetected, KeyFrameIndices, KeyFrameDetectionScores)
colnames(KeyFrameDF) = c("KeyFrameEvent", "AreKeyFramesDetected", "KeyFrameIndices", "KeyFrameDetectionScores")
View(KeyFrameDF)
