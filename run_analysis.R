##1. Preparation

setwd("G:/R")
#package used
install.packages("dplyr")
library(dplyr)
install.packages("data.table")
library(data.table)

#reading data
features <- read.table("UCI HAR Dataset/features.txt", header = FALSE)
activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
subjecttrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activitytrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featurestrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
subjecttest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activitytest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featurestest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

#assign column names to the data imported
colnames(activitylabels) <- c('activityid', 'activitylables')
colnames(subjecttrain) <- "subjectid"
colnames(featurestrain) <- features[, 2]
colnames(activitytrain) <- "activityid"
colnames(subjecttest) <- "subjectid"
colnames(featurestest) <- features[, 2]
colnames(activitytest) <- "activityid"
##2. Merge the train and the test data sets to creats one data set.
train <- cbind(activitytrain, subjecttrain, featurestrain)
test <- cbind(activitytest, subjecttest, featurestest)
data <- rbind(train, test)
colnames <- colnames(data)

##3. Extract only the measurements on the mean and standard deviation for each measurement.
columnwithmeanstd <- (grepl("activity..", colnames) | grepl("subject..", colnames) 
                      | grepl("-mean..", colnames) & !grepl("-meanFreq..",colnames) 
                      & !grepl("mean..-",colnames) | grepl("-std..",colnames) 
                      & !grepl("-std()..-",colnames))
data <- data[columnwithmeanstd == TRUE]

##4. Use descriptive activity names to name the activities in the data set
data <- merge(data, activitylabels, by ='activityid', all.x = TRUE)
colnames <- colnames(data)

## 5. appropriately label the data set with descriptive variable names.

for (i in 1:length(colnames)) 
{
  colnames[i] = gsub("\\()", "", tolower(colNames[i]))
  colNames[i] = gsub("-std$","stdev", tolower(colNames[i]))
  colNames[i] = gsub("-mean","mean", tolower(colNames[i]))
  colNames[i] = gsub("^(t)","time",tolower(colNames[i]))
  colNames[i] = gsub("^(f)","freq",tolower(colNames[i]))
  colNames[i] = gsub("([Gg]ravity)","gravity",tolower(colNames[i]))
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","body",tolower(colNames[i]))
  colNames[i] = gsub("[Gg]yro","gyro",tolower(colNames[i]))
  colNames[i] = gsub("AccMag","accmagnitude",tolower(colNames[i]))
  colNames[i] = gsub("([Bb]odyaccjerkmag)","bodyaccJerkmagnitude",tolower(colNames[i]))
  colNames[i] = gsub("JerkMag","jerkmagnitude",tolower(colNames[i]))
  colNames[i] = gsub("GyroMag","gyromagnitude",tolower(colNames[i]))
}

colnames(data) <- colnames

## 6. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

extracteddataactivitylabels <- data[, names(data) != 'activitylabels']
tidydata <- aggregate(extracteddataactivitylabels[, names(extracteddataactivitylabels) 
                                                  != c('activityid','subjectid')], 
                      by=list(activityid = extracteddataactivitylabels$activityid, 
                              subjectid = extracteddataactivitylabels$subjectid),mean)
tidydata = merge(tidydata,activitylabels,by='activityid',all.x=TRUE);
write.table(tidydata, file = "Tidy.txt", row.names = FALSE)