## Functions
getMeasurementsIndex <- function(features) {
  grep("-mean\\()|-std\\()",features$V2,ignore.case=FALSE)
}
  
nameActivity <- function(x, activity_names) {
  activity_names[x]
}

replaceVariablesNames <- function(X, indexes, features) {
  for(i in seq_along(indexes)) {
    names(X)[i+1] <- as.character(features$V2[indexes[i]])
  }
  X
}

## Reading Datasets
activity_labels  <- read.table("UCI HAR Dataset/activity_labels.txt")

## Reading Training Datasets
X_train  <- read.table("UCI HAR Dataset/train/X_train.txt")
Y_train  <- read.table("UCI HAR Dataset/train/Y_train.txt")
subject_train  <- read.table("UCI HAR Dataset/train/subject_train.txt")

## Reading Testing Datasets
X_test  <- read.table("UCI HAR Dataset/test/X_test.txt")
Y_test  <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test  <- read.table("UCI HAR Dataset/test/subject_test.txt")

## 1 - Merges the training and the test sets to create one data set

## Check dimensions
dim(X_train)
dim(X_test)
## Append datasets rows
X <- rbind(X_train,X_test)
## Check dimensions after append
dim(X)

## 2 - Extracts only the measurements on the mean and standard deviation
## for each measurement

## Read features variables in a table
## Find which variables are needed in features.txt
## the ones whichs contains substrings "-mean()" or "-svd()"
features <- read.table("UCI HAR Dataset/features.txt")
measures_variables <- getMeasurementsIndex(features)

## Extracts the columns needed subsetting by columns in vector vars
X <- X[,measures_variables]

## 3 - Uses descriptive activity names to name the activities 
## in the data set
y <- rbind(Y_train, Y_test)
activity <- sapply(y$V1, function(x) nameActivity(x,activity_labels$V2))
X <- cbind(activity, X)

## 4 - Appropriately labels the data set with descriptive variable names
X <- replaceVariablesNames(X, measures_variables, features)

## 5 -  creates a second, independent tidy data set with the average of each 
##      variable for each activity and each subject

## append the subject column variable until now not needed
subject <- rbind(subject_train, subject_test)
X <- cbind(subject = subject$V1, X)
library("reshape2")
Xmelt <- melt(X, id = c("subject","activity"))
tidyData <- dcast(Xmelt, subject + activity ~ variable, mean)

## Write the Dataset to a file
write.table(tidyData, "tidyData.txt", sep = " ", row.names = FALSE)
