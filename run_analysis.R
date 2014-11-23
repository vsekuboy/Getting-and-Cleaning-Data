# This script does the following:
#
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation
#    for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.


# Designate data directory and the data file names
#
DataDir <- "/Users/vijjisekuboyina/Coursera/Course3-DataClean/data/UCIHARDataset"

TrainDataDir <- sprintf ("%s/%s", DataDir, "train")
TestDataDir <- sprintf ("%s/%s", DataDir, "test")

FeaturesDat <- sprintf ("%s/%s", DataDir, "features.txt")
ActivityLblsDat <- sprintf ("%s/%s", DataDir, "activity_labels.txt")

# Read data related to Trainining and test
# Read Training Data
SubTrainDat <- sprintf ("%s/%s", TrainDataDir, "subject_train.txt")
xTrainDat <- sprintf ("%s/%s", TrainDataDir, "x_train.txt")
yTrainDat <- sprintf ("%s/%s", TrainDataDir, "y_train.txt")

# Read Test Data
SubTestDat <- sprintf ("%s/%s", TestDataDir, "subject_test.txt")
xTestDat <- sprintf ("%s/%s", TestDataDir, "x_test.txt")
yTestDat <- sprintf ("%s/%s", TestDataDir, "y_test.txt")

# Import Data from text files into memory
#
# Features data has the labels for column names of data in 'x' data
FeaturesTbl <- read.table (FeaturesDat)


# Import Training Data and merge them into one table
SubTrainTbl <- read.table (SubTrainDat)
colnames (SubTrainTbl) <- "Subject"
xTrainTbl <- read.table (xTrainDat)
colnames (xTrainTbl) <- FeaturesTbl [, 2]
yTrainTbl <- read.table (yTrainDat)
colnames (yTrainTbl) <- "ActID"
TrainingTbl <- cbind (yTrainTbl, SubTrainTbl, xTrainTbl)

# Import Test Data and merge then into one table
SubTestTbl <- read.table (SubTestDat)
colnames (SubTestTbl) <- "Subject"
xTestTbl <- read.table (xTestDat)
colnames (xTestTbl) <- FeaturesTbl [, 2]
yTestTbl <- read.table (yTestDat)
colnames (yTestTbl) <- "ActID"
TestTbl <- cbind (yTestTbl, SubTestTbl, xTestTbl)

##################################################################
# 1. Merge the training and the test sets to create one data set.#
##################################################################

AllDataTbl <- rbind (TrainingTbl, TestTbl)

##################################################################
# 2. Extract only the measurements on the mean and standard      #
#    deviation for each measurement.                             #
##################################################################

# Get label names of the table having all data
LabelNames <- colnames (AllDataTbl)
DataMeanStd <- AllDataTbl[, grepl("Subject|ActID|mean|std", LabelNames)]

###########################################################################
# 3. Use descriptive activity names to name the activities in the data set#
###########################################################################

# Read the activity data
ActivityTbl <- read.table (ActivityLblsDat)
colnames (ActivityTbl) <- c("ActID", "Activity")

# Add new column with name ActName for describing the activity
DataMeanStd$Activity <- ActivityTbl [DataMeanStd$ActID, 2]
# Remove the ActID column
DataMeanStd <- DataMeanStd [,-1]
# Move the new column Activity to the beginning
col<-grep ("Activity", names(DataMeanStd))
DataMeanStd <- DataMeanStd [, c(col, (1:ncol(DataMeanStd))[-col])]

########################################################################
# 4. Appropriately label  the data set with descriptive variable names.# 
########################################################################
DataLabels <- colnames (DataMeanStd)
for (i in 1:length(DataLabels)) {
  DataLabels[i] <- gsub("-mean\\()", " Mean", DataLabels[i]) 
  DataLabels[i] <- gsub("-std\\()", " Std Deviation", DataLabels[i]) 
  DataLabels[i] <- gsub("-meanFreq\\()", " Mean Frequency", DataLabels[i])
  DataLabels[i] <- gsub("^t", "Time: ", DataLabels[i])
  DataLabels[i] <- gsub("^f", "Freq: ", DataLabels[i])
  DataLabels[i] <- gsub("Acc", " Accelaration", DataLabels[i])
  DataLabels[i] <- gsub("BodyBody", "Body", DataLabels[i])
  DataLabels[i] <- gsub("Mag", " Magnitude", DataLabels[i])
  DataLabels[i] <- gsub("Gyro", " Gyro", DataLabels[i])
  DataLabels[i] <- gsub("Jerk", " Jerk", DataLabels[i])
}
colnames(DataMeanStd) <- DataLabels

###########################################################################
#From the data set in step 4, creates a second, independent tidy data     #
#set with the average of each variable for each activity and each subject.#
###########################################################################

require(plyr)

TidyDataWithAvgs <- ddply(DataMeanStd, c("Subject", "Activity"), numcolwise(mean))
write.table (TidyDataWithAvgs, file="tidydatawithaverages.txt", row.name=FALSE)