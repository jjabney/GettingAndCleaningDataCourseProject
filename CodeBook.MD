Code Book

Raw Data Source
======================================================================================
The raw data for this project is accelerometer data collected from the Samsung Galaxy S smartphone

Data package: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
Code Book: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

File descriptions:
Training 
Test
Labels
Features



Transformation
========================================================================================

Rename columns:
1. Assemble and rename
  "Acc" -> "Accelerometer"
  "Gyro" -> "Gyroscope"
  "BodyBody" -> "Body"
 "Mag" -> "Magnitude"
  "^t" -> "Time"
  "^f" -> "Frequency"
  "tBody" -> "TimeBody"
  "-mean()" -> "Mean"
  "-std()" -> "STD"
  "-freq()" -> "Frequency"
  "angle" -> "Angle"
  "gravity" -> "Gravity"
  
  2. filter only activity subject std and mean variables
  3. average for each activity and subject.
  4. order by subject and activity

