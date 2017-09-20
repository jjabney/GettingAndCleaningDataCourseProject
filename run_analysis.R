run_analysis<- function()
{

  library("dplyr")
  library("stringr")
  library("data.table")

  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  
  destFile <- "UCI_HAR_Dataset.zip"
  
  zip <-  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","FUCIHARDataset.zip")
  
  unzip(destFile)

  #load feature names
  featNames <- read.table("~/UCI HAR Dataset/features.txt")

  
  View(featNames)
  
  #load activity labels
  actLabels <- read.table("~/UCI HAR Dataset/activity_labels.txt", header = FALSE)
  
  
  #load training data
  subjTrng <- read.table("~/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
  actTrng <- read.table("~/UCI HAR Dataset/train/y_train.txt", header = FALSE)
  featTrng <- read.table("~/UCI HAR Dataset/train/X_train.txt", header = FALSE)
  
  #load test data
  subjTest <- read.table("~/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
  actTest <- read.table("~/UCI HAR Dataset/test/y_test.txt", header = FALSE)
  featTest <- read.table("~/UCI HAR Dataset/test/X_test.txt", header = FALSE)
  
  #bind training and test data
  subj <- rbind(subjTrng, subjTest)
  act<- rbind(actTrng, actTest)
  feat <- rbind(featTrng, featTest)
  
  #name columns
  colnames(feat) <- t(featNames[2])
  colnames(act) <- "Activity"
  colnames(subj) <- "Subject"

  #construt observations
  obs <- cbind(subj,act,feat)
  
  #make observation columns more intention revealing
  names(obs)<-gsub("Acc", "Accelerometer", names(obs))
  names(obs)<-gsub("Gyro", "Gyroscope", names(obs))
  names(obs)<-gsub("BodyBody", "Body", names(obs))
  names(obs)<-gsub("Mag", "Magnitude", names(obs))
  names(obs)<-gsub("^t", "Time", names(obs))
  names(obs)<-gsub("^f", "Frequency", names(obs))
  names(obs)<-gsub("tBody", "TimeBody", names(obs))
  names(obs)<-gsub("-mean()", "Mean", names(obs), ignore.case = TRUE)
  names(obs)<-gsub("-std()", "STD", names(obs), ignore.case = TRUE)
  names(obs)<-gsub("-freq()", "Frequency", names(obs), ignore.case = TRUE)
  names(obs)<-gsub("angle", "Angle", names(obs))
  names(obs)<-gsub("gravity", "Gravity", names(obs))

  #reduce observation columns to only the activity, subject, standard deviation & mean values
  obs <- obs[grep("(activity|subject|std|mean)",names(obs),ignore.case = TRUE)]

  #tidy data set average for each activity and subject.
  tidyData <- aggregate(. ~Subject + Activity, obs, mean)
  
  #order data
  tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
  
  #write to file
  write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
 

}

run_analysis()
