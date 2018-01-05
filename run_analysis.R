#The following script merges the training and the test sets from 'UCI HAR 
#Dataset' to create one complete dataset. The script only searches for the 
#values of the mean and standard deviation for each variable and uses 
#descriptive 'Activity' names as headers for the activities in the dataset. In 
#addition the script labels the data set with descriptive variable names. 
#Finally from the dataset from step 3 (see below), to create a second,
#independent tidy data set with the average of each variable for each activity
#and each subject.

## Precursor Setup
# Load the following package:
library(reshape2)

# Download and unzip the dataset if the file does not already exist on hardrive.
filename <- "getdata_dataset.zip"
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

## Step 1
# Load activity labels and features from dataset.
activity_Labels <- read.table("UCI HAR Dataset/activity_labels.txt")
activity_Labels[,2] <- as.character(activity_Labels[,2])

features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- as.character(features[,2])

## Step 2a Extract only the data on mean and standard deviation using regular
#expressions and the '|' or function.
features_headers <- grep(".*mean.*|.*std.*", features[,2])
features_headers.names <- features[features_headers,2]
# Again using regular expressions to find and replace characters within
# 'features' to look more presentable.
features_headers.names <- gsub('-mean', 'Mean', features_headers.names)
features_headers.names <- gsub('-std', 'Std', features_headers.names)
features_headers.names <- gsub('[-()]', '', features_headers.names)

## Step 2b
# Load the datasets from the files 'X_train.txt' and X_test.txt

X_train <- read.table("UCI HAR Dataset/train/X_train.txt")[features_headers]
train_Activities <- read.table("UCI HAR Dataset/train/Y_train.txt")
train_Subjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
# N.B.  Vectors for 'Activities' and  'Subjects' should be equal before continuing.
X_train <- cbind(train_Subjects, train_Activities, X_train)

X_test <- read.table("UCI HAR Dataset/test/X_test.txt")[features_headers]
test_Activities <- read.table("UCI HAR Dataset/test/Y_test.txt")
test_Subjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
# N.B.  Vectors for 'Activities' and  'Subjects' should be equal before continuing.
X_test <- cbind(test_Subjects, test_Activities, X_test)

## Step 2c
# Merge 'test' and 'train' datasets and add labels.
allData <- rbind(X_train, X_test)
colnames(allData) <- c("subject", "activity", features_headers.names)

## Step 3
# Convert activities & subjects into sorting variables via indexing.
allData$activity <- factor(allData$activity, levels = activity_Labels[,1], labels = activity_Labels[,2])
allData$subject <- as.factor(allData$subject)

## Step 4
# Create a second, independent 'tidy' dataset with the average of each variable
# for each activity and each subject.
allData.melted <- melt(allData, id = c("subject", "activity"))
allData.mean <- dcast(allData.melted, subject + activity ~ variable, mean)

# Write new 'tidy' dataset to a specified .txt file.
write.table(allData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)

## END