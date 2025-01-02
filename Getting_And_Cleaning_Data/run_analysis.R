# Getting and Cleaning Data Project John Hopkins Coursera

# Student: Tung NGUYEN
#use data.table package
library(data.table)
dataFolder <- "Getting_And_Cleaning_Data/data"
curPath <- getwd()
dataURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(dataURL, file.path(curPath, dataFolder, "Ass4Dataset.zip"))
unzip(zipfile = file.path(curPath, dataFolder, "Ass4Dataset.zip"), exdir=file.path(curPath, dataFolder))

dataPath =file.path(curPath, dataFolder)

# extract the mean and standard deviation of each measurements from activity labels + features
activityLabels <- fread(file.path(dataPath, "UCI HAR Dataset/activity_labels.txt"), col.names = c("classLabels", "activityName"))
features <- fread(file.path(dataPath, "UCI HAR Dataset/features.txt"), col.names = c("index", "featureNames"))
selectedFeatures <- grep("(mean|std)\\(\\)", features[, featureNames])
filteredMeasurements <- features[selectedFeatures, featureNames]
filteredMeasurements <- gsub('[()]', '', filteredMeasurements)

loadDataset <- function(dataPath, setType, features, measurements)
{
  dset <- fread(file.path(dataPath, paste("UCI HAR Dataset/", setType, "/X_", setType, ".txt", sep='')))[, features, with = FALSE]
  data.table::setnames(dset, colnames(dset), measurements)
  
  activities <- fread(file.path(dataPath, paste("UCI HAR Dataset/", setType, "/Y_", setType, ".txt", sep='')), col.names = c("Activity"))
  subjects <- fread(file.path(dataPath, paste("UCI HAR Dataset/", setType, "/subject_", setType, ".txt", sep='')), col.names = c("SubjectNum"))
  dset <- cbind(subjects, activities, dset)
  return(dset)
}

# Load train datasets with only the measurment
trainSet <- loadDataset(dataPath, "train", selectedFeatures, filteredMeasurements)
# Load test datasets with only the measurment
testSet <- loadDataset(dataPath, "test", selectedFeatures, filteredMeasurements)

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
combinedSet <- rbind(trainSet, testSet)

# Convert classLabels to activityName basically. More explicit. 
combinedSet[["Activity"]] <- factor(combinedSet[, Activity]
                                    , levels = activityLabels[["classLabels"]]
                                    , labels = activityLabels[["activityName"]])

combinedSet[["SubjectNum"]] <- as.factor(combinedSet[, SubjectNum])
combinedSet <- reshape2::melt(data = combinedSet, id = c("SubjectNum", "Activity"))
combinedSet <- reshape2::dcast(data = combinedSet, SubjectNum + Activity ~ variable, fun.aggregate = mean)

data.table::fwrite(x = combinedSet, file = file.path(dataPath, "tidyData.txt"), quote = FALSE)


